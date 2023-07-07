package it.unibo.cpatterns


// NB: do not nest into class of program, otherwise different instances cannot be compared, leading to subtle bugs!
trait State
object State {
  case object InitiatingState extends State

  case class MonitoringState(k: Int) extends State

  case class ExpandingState(k: Int) extends State

  case object WorkingState extends State

  case object ClosingState extends State

  val Initiating: State = InitiatingState

  def Monitoring(k: Int): State = MonitoringState(k)

  def Expanding(k: Int): State = ExpandingState(k)

  val Working: State = WorkingState
  val Closing: State = ClosingState
}

class StateBasedCollectiveBehaviour extends SimulatedAggregateProgram {

  import State._

  lazy val DISTANCE = node.get[Int]("connection_range")

  override def main(): Any = {
    val INTERVAL = 20
    val ENOUGH = 50
    val anchor: Boolean = mid() == 50

    val maps = spawn[Int,Unit,Unit](pid => args => {
      node.put("p_run_time", timestamp())
      val leader = mid() == pid
      val g = distanceTo(leader)
      node.put("g", g)
      val (state, n): (State, Int) = workflow[State,Int](leader, Initiating, 1) {
        case (Initiating, _) => {
          (Expanding(1), 1)
        }
        case s @ (ExpandingState(k), r) => {
          // node.put("nbrs", foldhood[Set[Int]](Set.empty)(_++_){ nbr(Set(mid()))})
          val maxExtension = C[Double, Double](g, Math.max, g, g)
          node.put("leader", leader)
          node.put("max_extension", maxExtension)
          if(maxExtension >= (k - 1) * DISTANCE) (Monitoring(k), k) else (ExpandingState(k), k)
        }
        case (s @ MonitoringState(k), r) => {
          val g = distanceTo(leader)
          node.put("nbrs", foldhood[Set[Int]](Set.empty)(_++_){ nbr(Set(mid()))})
          val countMembers = C[Double,Int](g, _ + _, 1, 0)  // TODO: count is not fine!
          // When the counting of members stabilises, then start counting rounds; then, move to "Closing" as soon as the counting exceeds a threshold
          node.put("count_members", countMembers) // TODO: count is not fine!
          val countRounds = rep[(Int,Int)]((countMembers, 0)){ case (m, k) => (countMembers, if(countMembers == m) k + 1 else k) }._2
          if(leader && countRounds >= INTERVAL) {
            if(countMembers >= ENOUGH) {
              (Closing, k)
            } else {
              (Expanding(k + 1), k + 1)
            }
          } else {
            (s, k)
          }
        }
        case s @ (ClosingState, _) => {
          print(".")
          s
        }
        case s => {
          print("#")
          s
        }// do nothing
      }
      node.put("state", state.hashCode())
      // node.put("state_history", state :: node.getOrElse("state_history", List.empty))
      node.put("n", n)
      ((), state != Closing & g < (n + 1) * DISTANCE)
    }, if(anchor) Set(mid()) else Set.empty, {})

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1)._1.hashCode()) % 100)
    } else {
      removeMolecule("pid");
      // removeMolecule("g");
      removeMolecule("state");
      // removeMolecule("n");
    }

    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
    node.put("context", vm.context.exports())
    node.put("export", vm.`export`)
  }

  def workflow[S, E](leader: Boolean, initialState: S, initialResult: E)(f: (S,E) => (S,E)): (S,E) = {
    val g = distanceTo(leader)
    rep[(S, E)]((initialState, initialResult)) { case (currState, r) => {
      val state = broadcastOn(g, currState)
      node.put(node.getOrElse("pid", -1).toString + "_state", state)
      align(state) { s => f(s, r) } // NB: aligning on 'state' creates weird bugs (?!)
    } }
  }

  def branchOn[V, O](v: V)(f: V => O): O =
    align(v) {
      f(_)
    }

  def broadcastOn[V](g: Double, v: V): V =
    G_along[V](g, nbrRange _, v, v => v)
}
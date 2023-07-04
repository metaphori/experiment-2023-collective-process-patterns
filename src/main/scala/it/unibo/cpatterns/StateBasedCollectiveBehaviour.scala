package it.unibo.cpatterns

class StateBasedCollectiveBehaviour extends SimulatedAggregateProgram {
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

  import State._

  override def main(): Any = {
    val DISTANCE = 50
    val INTERVAL = 10
    val ENOUGH = 30
    val anchor: Boolean = mid() == 50

    val maps = spawn[Int,Unit,Unit](pid => args => {
      val leader = mid() == pid
      val g = distanceTo(leader)
      val (state, n): (State, Int) = workflow[State,Int](leader, Initiating, 1) {
        case (Initiating, _) => {
          (Expanding(1), 1)
        }
        case (s @ ExpandingState(k), r) => {
          val maxExtension = C[Double, Double](g, Math.max, g, g)
          node.put("leader", leader)
          node.put("max_extension", maxExtension)
          if(maxExtension >= k) (Monitoring(k), r) else (s, r)
        }
        case (s @ MonitoringState(k), r) => {
          val countMembers = C[Double,Int](g, _+_, 1, 0)
          // When the counting of members stabilises, then start counting rounds; then, move to "Closing" as soon as the counting exceeds a threshold
          val countRounds = rep[(Int,Int)]((countMembers, 0)){ case (m, k) => (countMembers, if(countMembers == m) k + 1 else k) }._2
          if(countRounds >= INTERVAL) {
            if(countMembers >= ENOUGH) {
              (Closing, r)
            } else {
              (Expanding(k + 1), k + 1)
            }
          } else {
            (s, r)
          }
        }
        case s => {
          s
        } // do nothing
      }
      node.put("n", n)
      ((), state != Closing & distanceTo(leader) < (n + 1) * DISTANCE)
    }, if(anchor) Set(mid()) else Set.empty, {})

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1)._1.hashCode()) % 100)
    } else {
      removeMolecule("pid");
      removeMolecule("g");
    }

    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }

  def workflow[S, E](leader: Boolean, initialState: S, initialResult: E)(f: (S,E) => (S,E)): (S,E) = {
    val g = distanceTo(leader)
    rep[(S, E)]((initialState, initialResult)) { case (currState, r) => {
      val state = broadcastOn(g, currState)
      node.put(node.getOrElse("pid", -1).toString + "_state", currState)
      align(currState) { s => f(s, r) } // NB: aligning on 'state' creates weird bugs (?!)
    } }
  }

  def branchOn[V, O](v: V)(f: V => O): O =
    align(v) {
      f(_)
    }

  def broadcastOn[V](g: Double, v: V): V =
    G_along[V](g, nbrRange _, v, v => v)
}
package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

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

/**
 * This collective behaviour follows the state-based pattern.
 * The goal for the ensemble is to gather a sufficient amount of resources by expanding the
 *  process area in phases. A parameter `k` (initially set to 1) is used to denote the
 *  level of expansion (the radius is `k * DISTANCE`, where `DISTANCE` is the max communication range among neighbours).
 *  If at the current level of expansion (`MONITORING` state) the leader determines that the number of process members is not enough,
 *  it increments `k` to gather more (and enters the `EXPANSION` state).
 *  If a threshold of member count is exceeded, then the process terminates (`CLOSING` state)).
 */
class StateBasedCollectiveBehaviour extends SimulatedAggregateProgram {

  import State._

  lazy val CONNECTION_RANGE = node.get[Int]("connection_range")

  case class Pid(leaderId: ID, timestamp: Long)(
    val resourceTarget: Int,
    val expansionFactor: Double = 1.0,
    val numRoundsOfStability: Int = 10) {
    def distance = expansionFactor * CONNECTION_RANGE
    override def toString: String = s"Pid($leaderId, $timestamp)(res=$resourceTarget, expF=$expansionFactor, nRoundsStab=$numRoundsOfStability)"
  }

  lazy val procs: Set[Pid] = node.get[Map[Int, (Int, Int, Double, Int)]]("procs")
    .map(x => Pid(x._1, x._2._1)(x._2._2, x._2._3, x._2._4)).toSet

  override def main(): Any = {
    // Parse procs into Pid objects
    val notReentring = node.getOrElse[Set[Pid]]("all_pids", Set.empty).find(_.leaderId == mid()).isEmpty
    val pidSet = procs.filter(p => p.leaderId == mid() && p.timestamp == alchemistTimestamp.toDouble.toLong)
    if(!pidSet.isEmpty) print(pidSet)
    val anchor: Boolean = !pidSet.isEmpty && notReentring // the next condition is to avoid re-entrance

    val maps = spawn[Pid,Unit,Unit](pid => args => {
      node.put("p_run_time", alchemistTimestamp.toDouble.toLong)
      val leader = mid() == pid.leaderId
      val pround = rep(0)(_+1)
      // val nbrcount = foldhood(0)(_+_) { 1 }
      // if(leader) print(s"\n{m=${mid}}[${pid}](r=${pround})(nbrs=${nbrcount})")
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
          if(maxExtension >= (k - 1) * pid.distance) (Monitoring(k), k) else (ExpandingState(k), k)
        }
        case (s @ MonitoringState(k), r) => {
          val g = distanceTo(leader)
          node.put("nbrs", foldhood[Set[Int]](Set.empty)(_++_){ nbr(Set(mid()))})
          val countMembers = C[Double,Int](g, _ + _, 1, 0)  // TODO: count is not fine!
          // When the counting of members stabilises, then start counting rounds; then, move to "Closing" as soon as the counting exceeds a threshold
          node.put("count_members", countMembers) // TODO: count is not fine!
          val countRounds = rep[(Int,Int)]((countMembers, 0)){ case (m, k) => (countMembers, if(countMembers == m) k + 1 else k) }._2
          if(leader && countRounds >= pid.numRoundsOfStability) {
            if(countMembers >= pid.resourceTarget) {
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
      val withinProcess = state != Closing && g < (n + 1) * pid.distance
      val leaderNotReentering = if(leader && pround == 1) notReentring else true // if(leader && pround == 1 && nbrcount > 1) false else true
      ((), withinProcess && leaderNotReentering)
    }, if(anchor) pidSet else Set.empty, {})

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.leaderId)._1.hashCode()) % 100)
    } else {
      removeMolecule("pid")
      removeMolecule("g")
      removeMolecule("leader")
      removeMolecule("state")
      // removeMolecule("n")
    }

    node.put("pids", maps.keySet)
    node.put("all_pids", node.getOrElse("all_pids", Set.empty) ++ maps.keySet)
    node.put("numPids", maps.size)
    node.put("context", vm.context.exports())
    node.put("export", vm.`export`)
  }

  def workflow[S, E](leader: Boolean, initialState: S, initialResult: E)(f: (S,E) => (S,E)): (S,E) = {
    val g = distanceTo(leader)
    val s = rep[(S, E)]((initialState, initialResult)) { case (currState, r) => {
      // if(leader) { print(currState) }
      val state = broadcastOn(g, currState)
      node.put(node.getOrElse("pid", -1).toString + "_state", state)
      align(state) { s => f(s, r) } // NB: aligning on 'state' creates weird bugs (?!)
    } }
    // if(leader) { print(" -> " + s._1 + "; ") }
    s
  }

  def branchOn[V, O](v: V)(f: V => O): O =
    align(v) {
      f(_)
    }

  def broadcastOn[V](g: Double, v: V): V =
    G_along[V](g, nbrRange _, v, v => v)
}
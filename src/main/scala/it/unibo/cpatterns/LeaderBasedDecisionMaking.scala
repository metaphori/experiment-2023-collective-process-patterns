package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class LeaderBasedDecisionMaking extends SimulatedAggregateProgram with BlockT with BlockS {
  import it.unibo.cpatterns.LeaderBasedDecisionMaking._

  case class Pid(leaderId: ID)
  type Args = Unit

  override def main(): Any = {
    val grain = 750.0
    val isLeader = S(grain, nbrRange)
    val nodeLeader = broadcast(isLeader, mid())

    val procs = spawn[Pid,Args,Int](pid => args => {
      val status = pid.leaderId == nodeLeader
      val result = broadcast(mid()== pid.leaderId, pid.leaderId)
      (result, status)
    }, if(isLeader) Set(Pid(nodeLeader)) else Set.empty, ())

    node.put(EXPORT_IS_LEADER, isLeader)
    node.put(EXPORT_DECISION, procs.values.headOption.getOrElse(0))
  }

}

object LeaderBasedDecisionMaking {
  val EXPORT_IS_LEADER = "is_leader"
  val EXPORT_DECISION = "decision"
}
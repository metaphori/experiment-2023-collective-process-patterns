package it.unibo.scafi.examples

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.utils.MovementUtils

class ScrWithProcesses extends SimulatedAggregateProgram {
  import SpawnInterface._
  import ScrWithProcesses._

  type Args = Boolean
  type ProcOut = Double
  case class Pid(leaderId: Int)(val threshold: Double)

  def regionProcess(p: Pid)(stableLeader: Args): (ProcOut, Boolean) = {
    val isLeader = p.leaderId == mid() && stableLeader
    val g = distanceTo(isLeader, metric = nbrRange)
    val c = C[Double,Set[ID]](g, _++_, Set(mid()), Set.empty)
    val info = G[Set[ID]](isLeader, c, identity, metric = nbrRange)
    val head = G[ID](isLeader, mid(), identity, metric = nbrRange)
    node.put(Exports.GRADIENT, g)
    node.put(Exports.INCLUDED, if(isLeader) 1 else info.contains(mid()))
    node.put(Exports.COUNT, if(isLeader) c.size else 0)
    node.put(Exports.ISSUES, excludingSelf.anyHood(nbr { head } == head & nbr { info } != info))
    (g, if(isLeader) true else g < p.threshold)
  }

  /**
   * This program realises a simple implementation of the Self-organising Coordination Regions (SCR) pattern.
   */
  override def main(): Any = {
    // Sparse choice (leader election) of the cluster heads
    val leader = S(sense(Params.GRAIN), metric = nbrRange)
    val stableLeader = branch(leader) { rep(0)(_+1) } { 0 } > sense[Int](Params.LEADER_STABILITY_ROUNDS)
    val nodeSpecificThreshold = rep(sense[Int](Params.MIN_REGION_SIZE) 
      + scala.util.Random.nextInt(sense[Int](Params.MAX_REGION_SIZE) - sense[Int](Params.MIN_REGION_SIZE)))(identity)
    val generatedProcesses: Set[Pid] = if(stableLeader) Set(Pid(mid())(nodeSpecificThreshold)) else Set.empty
    val maps = spawn[Pid,Args,ProcOut](regionProcess, generatedProcesses, stableLeader)

    /*
    // G block to run a gradient from the leaders
    val g = distanceTo(leader, metric = nbrRange)
    // C block to collect information towards the leaders
    val c = C[Double,Set[ID]](g, _++_, Set(mid()), Set.empty)
    // G block to propagate decisions or aggregated info from leaders to members
    val info = G[Set[ID]](leader, c, identity, metric = nbrRange)
    val head = G[ID](leader, mid(), identity, metric = nbrRange)
    */

    node.put(Exports.LEADER, if(stableLeader) 1 else 0)

    if(maps.nonEmpty) {
      node.put(Exports.NPROCS, maps.size)
      node.put(Exports.PID, if(maps.size > 1) 0 else (Math.abs(maps.maxBy(_._1.leaderId)._1.hashCode())) % 100 + 1)
      node.put(Exports.GRADIENT, maps.maxBy(_._1.leaderId)._2)
    } else {
      removeMolecule(Exports.NPROCS)
      removeMolecule(Exports.PID)
      removeMolecule(Exports.GRADIENT)
      removeMolecule(Exports.INCLUDED)
      removeMolecule(Exports.COUNT)
      removeMolecule(Exports.ISSUES)
    }
  }

}
object ScrWithProcesses {
  object Exports {
    val NPROCS = "nprocs"
    val PID = "pid"
    val LEADER = "leader"
    val INCLUDED = "included"
    val COUNT = "count"
    val GRADIENT = "g"
    val ISSUES = "issues"
  }
  object Params {
    val GRAIN = "grain"
    val LEADER_STABILITY_ROUNDS = "leader_stability_rounds"
    val MIN_REGION_SIZE = "min_region_size"
    val MAX_REGION_SIZE = "max_region_size"
  }
}


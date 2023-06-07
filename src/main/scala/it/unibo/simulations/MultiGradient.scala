package it.unibo.simulations

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.utils.MovementUtils

class MultiGradient extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with BlockG with Gradients with MovementUtils {
  import SpawnInterface._

  case class Event(id: ID, from: Long, to: Long, ext: Int)
  def isSource: Boolean = getEvent.nonEmpty

  def getEvent: Option[Event] = {
    val procs: Set[Event] = node.get[Map[Int, (Int, Int, Int)]]("procs").map(x => Event(x._1, x._2._1, x._2._2, x._2._3)).toSet
    val t = alchemistTimestamp.toDouble.toLong
    procs.find(e => e.id == mid() && t >= e.from && t <= e.to)
  }

  /**def removeMolecule(name: String) = if(node.has(name)) node.remove(name)
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {

    case class Pid(source: ID)(val maxExtension: Double)
    def processLogic(pid: Pid)(source: Boolean): (Double,Boolean) = {
      val g = distanceTo(pid.source == mid() && source)
      (g, g <= pid.maxExtension)
    }
    val pids: Set[Pid] = if(isSource) Set(Pid(mid())(getEvent.get.ext)) else Set.empty

    val maps = spawn[Pid,Boolean,Double](processLogic, pids, isSource)

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.source)._1.hashCode()) % 100)
      node.put("g", maps.maxBy(_._1.source)._2)
    } else { removeMolecule("pid"); removeMolecule("g"); }

    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }

  // TODO: fix remove to perform the check
  def removeMolecule(name: String) = if(node.has(name)) node.remove(name)
}
package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point2D
import it.unibo.scafi.utils.MovementUtils
import org.apache.commons.math3.random.RandomGenerator

class MovingProcess extends SimulatedAggregateProgram {

  case class Pid(issuer: ID, from: Long, to: Long, x1: Int, y1: Int, x2: Int, y2: Int) {
    def within(p: Point2D): Boolean =
      p.x >= Math.min(x1, x2) && p.x <= Math.max(x1, x2) && p.y >= Math.min(y1,y2) && p.y <= Math.max(y1,y2)
  }

  case class CarrierPid(leader: ID)

  def issuing: Set[Pid] = {
    val procs: Set[Pid] = node.get[Map[Int, (Int, Int, Int, Int, Int, Int)]]("procs")
      .map(x => Pid(x._1, x._2._1, x._2._2, x._2._3, x._2._4, x._2._5, x._2._6)).toSet
    val t = alchemistTimestamp.toDouble.toLong
    procs.filter(p => p.from <= t && p.to >= t)
  }

  /**def removeMolecule(name: String) = if(node.has(name)) node.remove(name)
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {
    // TODO

    /*
    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.to)._1.hashCode()) % 100)
    } else { removeMolecule("pid"); removeMolecule("g"); }

    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
     */
  }
}
package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point2D
import it.unibo.scafi.utils.MovementUtils
import org.apache.commons.math3.random.RandomGenerator

class SpaceAttachedProcess extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with BlockG with Gradients with MovementUtils {

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
    val leader = mid() % 33 == 0
    // val gradient = distanceTo(leader)
    val carrier = broadcast(leader, mid())
    var pids: Set[Pid] = Set.empty
    val carriers = spawn[CarrierPid,Unit,Unit](pid => args => {
      pids = pids ++ gossip[Set[Pid]](issuing, _++_)
      ((), leader || pid.leader == carrier)
    }, if(leader) Set(CarrierPid(mid())) else Set.empty, {})
    val maps = spawn[Pid,Unit,Unit](pid => args => {
      ((), pid.within(currentPosition()))
    }, pids, ())

    if(!carriers.isEmpty) {
      node.put("carrier", Math.abs(carriers.maxBy(_._1.leader)._1.hashCode()) % 100)
    }
    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.to)._1.hashCode()) % 100)
    } else { removeMolecule("pid"); removeMolecule("g"); }

    direction(currentPosition(), direction = if(mid()%2==0) D.W else D.N, hstep = 10, vstep = 0)
    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }

  // TODO: fix remove to perform the check
  def removeMolecule(name: String) = if(node.has(name)) node.remove(name)

  def gossip[T](value: T, combine: (T,T) => T) = rep(value)(curr => foldhood(curr)(combine){ nbr(value) })

  def chooseOneAndKeep[T](rg: RandomGenerator, ts: T*): T = rep(Option.empty[T])(chosen => {
    chosen.orElse(Some(ts(rg.nextInt(ts.length))))
  }).get

  def chooseOneAndKeepForTime[T](rg: RandomGenerator, duration: Int, ts: T*): T = rep((Option.empty[(T,Long)]))(chosen => {
    chosen.filter(c => timestamp() - c._2 < duration).orElse(Some(ts(rg.nextInt(ts.length)), timestamp()))
  }).get._1
}
package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class TimeReplication extends SimulatedAggregateProgram with BlockT {
  import it.unibo.cpatterns.TimeReplication._

  def sources(): Map[ID,(Int,Int)] = node.get[Map[ID, (Int, Int)]](PARAM_SOURCES)
  lazy val REPLICATION_INTERVAL =node.get[Double](PARAM_REPLICATION_INTERVAL)
  lazy val NUM_REPLICATES = node.get[Int](PARAM_NUM_REPLICATES)

  override def main(): Any = {
    val currT = alchemistTimestamp.toDouble.toLong
    val activeSources = sources().filter(s => currT>= s._2._1 && currT <= s._2._2)
    val isSource = activeSources.contains(mid())
    node.put(EXPORT_IS_SOURCE, isSource)
    val gradient = distanceTo(isSource)
    val replicatedGradientReplica = replicated[Boolean,Double](distanceTo(_))(isSource, REPLICATION_INTERVAL, NUM_REPLICATES)
    node.put("replicated_gradients", replicatedGradientReplica)
    val replicatedGradient = replicatedGradientReplica.collectFirst(_._2).getOrElse(Double.PositiveInfinity)
    node.put(EXPORT_GRADIENT_VALUE, gradient)
    node.put(EXPORT_REPLICATED_GRADIENT_VALUE, replicatedGradient)
  }

  /**
   * It works by keeping 𝑘 running replicates of a collective computation
   * executing concurrently, each alive for a certain amount of time. New
   * instances are activated with interval 𝑝, staggered in time.
   */
  def replicated[A,R](proc: A=>R)(argument:A, p:Double, k:Int) = {
    val lastPid = clock(p, alchemistDeltaTime().toLong)
    spawn[Long,A,R](pid => arg => (proc(arg), pid > lastPid+k),
      Set(lastPid), argument)
  } // returns Map[Long,R] from replicate IDs to corresp. values

  def clock(len: Long, decay: Long): Long =
    rep((0L,len)){ case (k,left) => // Function by pattern matching
      branch (left == 0){ (k+1,len) }{ (k, T(len, decay).toLong) }
    }._1 // "_1" projects to the first element of the tuple

  /*
  def T(init: Double, floor: Double, decay: Double=>Double): Double =
    rep(init) { v => Math.min(init, Math.max(floor, decay(v))) }

  def T(init: Double, delta: Double): Double = T(init, 0.0, (t: Double) => t - delta))
   */

}

object TimeReplication {
  val PARAM_SOURCES = "sources"
  val PARAM_REPLICATION_INTERVAL = "replication_interval"
  val PARAM_NUM_REPLICATES = "number_of_replica"
  val EXPORT_GRADIENT_VALUE = "gvalue"
  val EXPORT_REPLICATED_GRADIENT_VALUE= "rgvalue"
  val EXPORT_IS_SOURCE = "isSource"
}
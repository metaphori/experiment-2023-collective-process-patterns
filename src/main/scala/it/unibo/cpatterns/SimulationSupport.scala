package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.utils.MovementUtils

trait SimulatedAggregateProgram extends AggregateProgram
  with StandardSensors
  with ScafiAlchemistSupport
  with CustomSpawn with BlockG with Gradients with MovementUtils
  with BlockC {

  def removeMolecule(name: String) = if(node.has(name)) node.remove(name)

  def broadcastAlong[T](g: Double, v: T): T =
    G_along[T](g, nbrRange, v, x => x)


  override def sspawn[K, A, R](process: K => A => POut[R], params: Set[K], args: A): Map[K,R] =
    spawn2[K,A,Option[R]](k => a => handleOutput(handleTermination(process(k)(a))), params, args)
      .collectValues { case Some(p) => p }

  import SpawnInterface._

  override def runOnSharedKeysWithShare[K, A, R](process: K => (R, Boolean), params: Set[K]): Map[K, R] =
    share(Map.empty[K, R])((loc, nbr) => {
      (includingSelf.unionHoodSet(nbr().keySet ++ params))
        .mapToValues(x => exportConditionally(process.apply(x)))
        .collectValues[R] { case (r, true) => r }
    })

  /*
  BEWARE: the interplay between asynchronicity and retention may cause termination failure.
  In those case, it might be better to use a kind of memory of process keys to avoid re-entrance,
  with a periodic garbage collection.
  %
  This version works e.g. with:
    - RandomDiracComb(max=1.0, min=1.5), retention = 2.0
    - ExponentialTime(1.0), retention= 2.0 | 5.0
 */
  override def handleTermination[T](out: POut[T]): POut[T] = {
    share[(Boolean,Int,POut[T])]((false,0,out)){
      case (loc,nbrd) =>
        val mustTerminate = includingSelf.anyHood(nbrd()._1) || out.status==Terminated
        val mustExit = includingSelf.everyHood(nbr { mustTerminate })
        (mustTerminate, 1, if(mustExit) POut(out.result, External) else if(mustTerminate) POut(out.result, Terminated) else out)
    }._3
  }
  /* the following one is BROKEN */
  /*
  override def handleTermination[T](out: POut[T]): POut[T] = {
    share[(Boolean,Int,POut[T])]((false,0,out)){
      case (loc,nbrd) =>
        val mustTerminate = includingSelf.anyHood(nbrd()._1) || loc._1 || out.status==Terminated
        val mustExit = includingSelf.everyHood(nbrd()._1) // || (mustTerminate && loc._2 == 0) // includingSelf.everyHood(nbr{mustTerminate})
        (mustTerminate, 1, if(mustExit) POut(out.result, External) else if(mustTerminate) POut(out.result, Terminated) else out)
    }._3
  }
  */
}

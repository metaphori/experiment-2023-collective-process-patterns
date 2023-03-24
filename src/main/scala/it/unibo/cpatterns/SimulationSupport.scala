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

  override def runOnSharedKeysWithShare[K, A, R](process: K => (R, Boolean), params: Set[K]): Map[K, R] =
    share(Map.empty[K, R])((loc, nbr) => {
      (includingSelf.unionHoodSet(nbr().keySet ++ params))
        .mapToValues(x => exportConditionally(process.apply(x)))
        .collectValues[R] { case (r, true) => r }
    })
}

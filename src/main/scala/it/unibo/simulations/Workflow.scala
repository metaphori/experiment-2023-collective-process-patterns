package it.unibo.simulations

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.utils.MovementUtils

class Workflow extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with BlockG with Gradients with MovementUtils {
  import SpawnInterface._

  /**
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {

    workflow(0, ""){
      case 1 => 2 -> "foo"
    }
  }

  type S = Int
  type O = String
  def workflow(initialState: S, initialOutput: O)(pf: Function[S,(S,O)]): O = {
    rep((initialState, initialOutput)) { case (s,o) => {
      pf(s)
    }}._2
  }

}
package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class ProlongedTermination extends SimulatedAggregateProgram {

  lazy val eventDetector = alchemistRandomGen.nextGaussian() > 2
  lazy val eventHandler = alchemistRandomGen.nextGaussian() > 1.5

  val BUBBLE_RADIUS = 200
  val BUBBLE_DURATION_ROUNDS = 50


  override def main(): Any = {
    case class Pid(leader: ID)
    case class Args()
    case class Return(terminate: Boolean = false)

    def processLogic(pid: Pid)(args: Args): (Return, Boolean) = {
      val source = pid.leader == mid()
      val status = distanceTo(source) < BUBBLE_RADIUS
      val k = rep(0)(_+1)
      val wannaTerminate = mid()%3==0 && k>BUBBLE_DURATION_ROUNDS
      (Return(!source && wannaTerminate), status)
    }

    val generator = mid() % 25 == 0;

    val proscription = rep[Set[Pid]](Set.empty)(proscription => {
      val map = spawn[Pid, Args, Return](pid => args => {
        branch(proscription.contains(pid)) {
          (Return(), false)
        }{
          processLogic(pid)(args)
        }
      }, if(generator) Set(Pid(mid())) else Set.empty, Args())

      if (!map.isEmpty) {
        node.put("pid", map.maxBy(_._1.leader)._1.leader % 100)
      } else {
        removeMolecule("pid")
        removeMolecule("g")
      }

      proscription ++ map.filter(_._2.terminate).keySet
    })
    node.put("proscription", proscription)
  }

}
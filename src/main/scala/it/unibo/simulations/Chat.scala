package it.unibo.simulations

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class Chat extends SimulatedAggregateProgram {
  import SpawnInterface._

  case class Pid(from: ID, to: ID, at: Long)
  case class ChatArgs(parentToCentre: ID, dependentNodes: Set[ID])

  def pids: Set[Pid] = {
    val t = alchemistTimestamp.toDouble.toLong
    node.get[Map[Int, (Int, Int)]]("procs")
      .map(x => Pid(x._2._1, x._2._2, x._1)).toSet.filter(p => p.from == mid() && (t == p.at || (t - alchemistDeltaTime(0.0).toLong <= p.at && t >= p.at)))
  }

  def chatProcessLogic(msg: Pid)
                      (args: ChatArgs): (String, Status) = {
    val inPathFromSrcToCentre = msg.from==mid() | includingSelf.anyHood {
      nbr(args.parentToCentre) == mid()
    }
    val gotMsg = broadcast(mid() == msg.to, mid() == msg.to)
    val inPathFromTargetToCentre = args.dependentNodes.contains(msg.to)
    val inRegion = inPathFromSrcToCentre || inPathFromTargetToCentre
    val status: Status = if(gotMsg && mid() == msg.from) Terminated else if (inRegion) { Output } else { External }

    (s"${msg.from}->${msg.to}", status)
  }

  def chat(centre: ID, pids: Set[Pid]): Map[Pid,String] = {
    val (distToCentre, parentToCentre) = distanceToWithParent(centre == mid())

    val dependentNodes = rep(Set.empty[ID]){ case (s: Set[ID]) =>
      excludingSelf.unionHoodSet[ID](mux( nbr{parentToCentre}==mid() ){ nbr(s) }{ Set.empty[ID] }) + mid()
    } // set of nodes whose path towards gen passes through me

    node.put("g", distToCentre)

    sspawn[Pid,ChatArgs,String](pid => args => chatProcessLogic(pid)(args),
        pids,
        ChatArgs(parentToCentre, dependentNodes))
  }

  override def main(): Any = {
    val centre = 250
    val maps = chat(centre, pids)

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.to)._1.hashCode()) % 100)
    } else { removeMolecule("pid"); }

    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }


  def distanceToWithParent(source: Boolean): (Double, ID) = {
    rep((Double.PositiveInfinity, -1)){ case (dist, parent) =>
      mux(source){
        (0.0, mid())
      }{
        excludingSelf.minHoodSelector(nbr{dist} + nbrRange()){
          (nbr{dist}+nbrRange(),nbr{ mid() })
        }.getOrElse((Double.PositiveInfinity, -1))
      }
    }
  }
}

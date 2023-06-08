package it.unibo.cpatterns

class NodeAttachedProcess extends SimulatedAggregateProgram {

  /**def removeMolecule(name: String) = if(node.has(name)) node.remove(name)
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {
    val SIZE = 80
    val anchor: Boolean = mid() == 50

    val maps = spawn[Int,Unit,Unit](pid => args => {
      ((), distanceTo(mid()==pid) < SIZE)
    }, if(anchor) Set(mid()) else Set.empty, {})

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1)._1.hashCode()) % 100)
    } else { removeMolecule("pid"); removeMolecule("g"); }

    rectangleWalk()
    if(anchor) direction(currentPosition(), direction = D.N, hstep = 2, vstep = 10)
    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }
}
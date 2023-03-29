package it.unibo.cpatterns

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.utils.MovementUtils

class EventTriggering extends SimulatedAggregateProgram {
  import SpawnInterface._

  lazy val eventDetector = alchemistRandomGen.nextGaussian() > 2
  lazy val eventHandler = alchemistRandomGen.nextGaussian() > 1.5

  val BUBBLE_RADIUS = 100

  val BUBBLE_DURATION_ROUNDS = 15
  val HANDLING_BUBBLE_DURATION_ROUNDS = 30


  override def main(): Any = {
    node.put("event-detector", eventDetector)
    node.put("event-handler", eventHandler)
    node.put("reporting", 0)

    case class Event(detector: ID)(val handler: Option[ID] = None)

    def detectEvents(): Set[Event] = {
      val value = rep(scala.util.Random.nextInt(100))(_ + 1) % 100
      node.put("value", value)
      if(value == 0) {
        //println(s"${mid()} got an event!")
        Set(Event(mid())(Some(mid())))
      } else { Set.empty }
    }

    def eventHandlerProcess(e: Event)(args: Unit): POut[Unit] = {
      val source = mid() == e.handler.getOrElse(e.detector)
      val ownT = rep(0)(_+1)
      val t = broadcast(source, if(source) ownT else 0)
      val g = distanceTo(source)
      node.put(s"${e}_g", g)
      val status = if(source && t >= HANDLING_BUBBLE_DURATION_ROUNDS) Terminated
        else if(g < BUBBLE_RADIUS && t < HANDLING_BUBBLE_DURATION_ROUNDS) Output
        else External
      POut((), status)
    }

    def reportEvents(events: Set[Event]): Set[Event] = {
      // if(eventHandler) events else Set.empty
      sspawn((event: Event) => (_:Unit) => {
        val leader = mid() == event.detector
        val g = distanceTo(leader)
        val withinBubble = g < BUBBLE_RADIUS
        val handlers = C[Double,Set[ID]](g, _++_, if(eventHandler) Set(mid()) else Set.empty, Set.empty)
        val NO_HANDLER = -1
        val decidedHandler = rep(NO_HANDLER)(h => {
          if(h != NO_HANDLER) h else handlers.maxOption.getOrElse(NO_HANDLER)
        })
        if(decidedHandler != NO_HANDLER  && leader) { println(s"${mid()} chose handler ${decidedHandler}") }
        val chosenHandler = broadcast(leader, decidedHandler)
        val eventToHandle = broadcast(leader, event)
        val ownT = rep(0)(_ + (if(chosenHandler != NO_HANDLER) 1 else 0))
        val t = broadcast(leader, if (leader) ownT else 0)
        POut(eventToHandle, /*if(leader && t==0 && events.isEmpty) External
        else*/ if(withinBubble && t <= BUBBLE_DURATION_ROUNDS) {
          node.put("reporting", 1)
          if(mid() == chosenHandler) { /* println(s"${mid} was chosen for events ${eventToHandle}!"); */ Output } else Bubble
        } else if(withinBubble && t > BUBBLE_DURATION_ROUNDS) Terminated else  External)
      }, events, {}).values.toSet
    }

    val detectedEvents: Set[Event] = mux(eventDetector){ detectEvents() } { Set.empty }
    node.put("events_detected", detectedEvents)
    node.put("events_detected_any", !detectedEvents.isEmpty)
    val eventsToHandle: Set[Event] = reportEvents(detectedEvents).map(_.copy()(handler = Some(mid())))
    node.put("events_to_handle", eventsToHandle)
    node.put("events_to_handle_any", !eventsToHandle.isEmpty)
    // if(!eventsToHandle.isEmpty) { println(s"${mid} has to handle ${eventsToHandle}") }

    val map = sspawn[Event, Unit, Unit](pid => args => eventHandlerProcess(pid)(args),
      eventsToHandle,
      {})

    if (!map.isEmpty) {
      //println(s"${mid} has map: ${map}")
      node.put("pid", Math.abs(map.maxBy(_._1.handler)._1.hashCode()) % 100)
    } else {
      removeMolecule("pid")
      removeMolecule("g")
    }
  }

}
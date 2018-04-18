package mud

import akka.actor.Actor
import akka.actor.ActorRef


class ActivityManager extends Actor{
  import ActivityManager._
  
  private val pq = new PriorityQueue[Event]((e1,e2) => e1.time<e2.time)
  private var currentTime = 0
  def receive = {
    case ScheduleEvent(delta:Int, message:Any) =>
      pq.enqueue(Event(currentTime+delta, sender, message))
    case DoEvents =>
      currentTime += 1
      while(!pq.isEmpty && pq.peek.time <= currentTime){
        val dEvent = pq.dequeue()
        dEvent.sender ! dEvent.message
      }
  }
}

object ActivityManager {
  case class Event(time:Int, sender:ActorRef, message:Any)
  // Tells the ActivityManager to schedule an event at delta after the current time to send back the specified message.
  case class ScheduleEvent(delta:Int, responseMessage:Any)
  // Tells the activity manager to check for events that need to be processed.
  case object DoEvents 
}
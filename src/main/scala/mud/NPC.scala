package mud

import akka.actor.Actor
import akka.actor.ActorRef

class NPC(name:String) extends Actor{
  private var currentRoom: ActorRef = null
  private var health:Int = 10
  
  import NPC._
  import Player._
  
  
  def receive = {
    case TakeExit(newRoom) =>
      newRoom match {
        case Some(nextRoom) =>
          if(currentRoom != null) currentRoom ! Room.PExit(self)
          currentRoom = nextRoom
          currentRoom ! Room.PEnter(self)
        case None => println("Error in room movement for NPC")
      }
    Main.actSuper ! ActivityManager.ScheduleEvent(20, Walk)
      //player move randomly among rooms
    case Walk => 
      currentRoom ! Room.GetExit(util.Random.nextInt(6))
      
    case PrintMessage(msg) =>
      //NPCs don't print messages
      
    case m => println("unknown message in NPC: " + m)
  }

}

object NPC{
  case object Walk
}
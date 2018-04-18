package mud

import akka.actor.Actor
import akka.actor.Props

class NPCSupervisor extends Actor{
  import NPCSupervisor._
  
 def receive = {
    case NewNPC(name, currentRoom)=>
      val newNPC = context.actorOf(Props(new NPC(name)), name)
      Main.roomSuper ! RoomSupervisor.Location(newNPC, currentRoom)
    
    case m => println(s"Bad message in PlayerSuper: $m")
  }
}

object NPCSupervisor {
  case object ProcessAllInput
  case class NewNPC(name:String, currentRoom: String)
}
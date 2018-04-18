package mud

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

class RoomSupervisor extends Actor {
  import RoomSupervisor._
  
  def receive = {
    case Location(player, currentRoom) => {
      player ! Player.TakeExit(rooms.get(currentRoom))
    }
    case m => println("unknown message in roomSupervisor: " + m)
  }

  val rooms = readRooms()
  context.children.foreach(_ ! Room.LinkExits(rooms))
  def readRooms() = {
    val xmlData = xml.XML.loadFile("RoomData.xml")
    (xmlData \ "room").map(n => {
      val name = (n \ "@name").text  
      val key = (n \ "@key").text
      val desc = (n \ "description").text
      val exits = (n \ "exits").text.split(",").padTo(6, "").map(_.trim)
      val items = (n \ "item").map(in => Item.apply(in)).toList
       (n \ "npc").foreach(in => Main.npcSuper ! NPCSupervisor.NewNPC((in\"@name").text, key))
      (key, context.actorOf(Props(new Room(name, desc, exits, items)), key))
    }).toMap
  }
}

object RoomSupervisor {
  case class Location(player: ActorRef, currentRoom: String)
}
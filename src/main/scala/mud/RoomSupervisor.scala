package mud

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

class RoomSupervisor extends Actor {
  import RoomSupervisor._
  private var _exits: scala.collection.mutable.Map[String, Array[String]] = scala.collection.mutable.Map[String, Array[String]]()
  
  def receive = {
    case Location(player, currentRoom) => {
      player ! Player.TakeExit(rooms.get(currentRoom))
    }
    case ShortPath(player, currentRoom, dest) =>
      player ! Player.PrintMessage(shortestPath(currentRoom, dest).mkString(" "))
      
    case GetExits(keyword, exitsN) =>
      _exits += (keyword -> exitsN)

    //Room ! Room.LinkExits
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
      (n \ "npc").foreach(in => Main.npcSuper ! NPCSupervisor.NewNPC((in \ "@name").text, key))
      (key, context.actorOf(Props(new Room(name, key, desc, exits, items)), key))
    }).toMap
  }
  
  private val directions = Array[String]("north", "south", "east", "west", "up", "down")

  def shortestPath(currentRoom: String, dest: String): List[String] = {
    def helper(d: String, visited: Set[String]): (Int, List[String]) = {
      if(d == dest) (0, List(dest)) else {
        val newVisited = visited + d
        var ret = (1000000000, List[String]())
        for ((d2,dir) <- _exits(d).zip(directions)) {
          if (!newVisited.contains(d2) && _exits.contains(d2)) {
            val tmp = helper(d2, newVisited)
            if (tmp._1 < ret._1) ret =(tmp._1,dir::tmp._2)
          }
        }
        (ret._1 + 1, d +: ret._2)
      }
    }
    helper(currentRoom, Set())._2
  }

}

object RoomSupervisor {
  case class Location(player: ActorRef, currentRoom: String)
  case class GetExits(keyword: String, exitsN: Array[String])
  case class ShortPath(player: ActorRef, currentRoom: String, dest: String)
}
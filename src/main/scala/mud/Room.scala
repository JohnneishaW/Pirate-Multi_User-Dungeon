package mud

import akka.actor.Actor
import akka.actor.ActorRef
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket

class Room(private val name: String,private val key:String, private val desc: String, val exitNames: Array[String], private var _items: List[Item]) extends Actor {
  import Room._

  private var exits: Array[Option[ActorRef]] = null
  private var allplayers = List[ActorRef]()

  def items = _items
  def receive = {
    case LinkExits(rooms) =>
      exits = exitNames.map(rooms.get)
      sender ! RoomSupervisor.GetExits(key,exitNames)
      println(name + " " + exits.length)
    case GetItem(itemName) =>
      sender ! Player.AddToInventory(getItem(itemName))
    case PrintDescription => sender ! Player.PrintMessage(description())
    case DropItem(item) =>
      dropItem(item)
    case GetExit(direct) =>
      sender ! Player.TakeExit(getExit(direct))
    case PEnter(player) =>
      allplayers.foreach(p => p ! Player.PrintMessage(player.path.name + " entered the room.\n"))
      allplayers ::= player
    case PExit(player) =>
      allplayers = allplayers.filter(_ != player)
      allplayers.foreach(p => p ! Player.PrintMessage(player.path.name + " left the room.\n"))
    case GetCharacter(playerName) =>
      sender ! Player.CharacterInRoom(getPlayer(playerName))
    case SendMessage(msg: String) =>
      for (p <- allplayers) p ! Player.PrintMessage(msg)
    case m => println("unknown message in room: " + m)
  }

  def description(): String = {
    val exitN = exitNames.toList.zip(List("north", "south", "east", "west", "up", "down"))
    name + "\n" + desc + "\n" + "Items: " + items.map(_.name).mkString(", ") + "\n" + "Exits: " + exitN.filter(_._1 != ("")).map(_._2).mkString(", ") + "\nPlayers: " + allplayers.map(_.path.name).mkString(", ")
  }

  def getExit(dir: Int): Option[ActorRef] = {
    exits(dir)
  }

  def getItem(itemName: String): Option[Item] = {
    val itemFound = items.find(_.name == itemName.toLowerCase)
    _items = _items.filterNot(_.name == itemName.toLowerCase)
    itemFound
  }
  
  def getPlayer(playerName: String): Option[ActorRef] = {
    val playerFound = allplayers.find(_.path.name == playerName)
    allplayers =  allplayers.filterNot(_.path.name== playerName)
    playerFound
  }
  def dropItem(item: Item): Unit = {
    _items = item :: _items
  }

}

object Room {
  case object PrintDescription
  case class SendMessage(msg: String)
  case class NewPlayer(name: ActorRef, currentRoom: String)
  case class GetItem(item: String)
  case class DropItem(item: Item)
  case class GetExit(direct: Int)
  case class PEnter(player: ActorRef)
  case class PExit(player: ActorRef)
  case class LinkExits(rooms: Map[String, ActorRef])
  case class GetCharacter(playerName: String)
}
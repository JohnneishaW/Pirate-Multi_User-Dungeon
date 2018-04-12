package mud

import akka.actor.Actor
import akka.actor.ActorRef

class Room(private val name: String, private val desc: String, val exitNames: Array[String], private var _items: List[Item]) extends Actor {
  import Room._

  private var exits: Array[Option[ActorRef]] = null

  def items = _items
  //any actions that go through the room happen in receive 
  def receive = {
    case LinkExits(rooms) => exits = exitNames.map(rooms.get)
    case GetItem(itemName) =>
      sender ! Player.AddToInventory(getItem(itemName))
    case PrintDescription => sender ! Player.PrintMessage(description())
    case DropItem(item) =>
      dropItem(item)
    case GetExit(direct) =>
      sender ! Player.TakeExit(getExit(direct))
    case m => println("unknown message in room: " + m)
  }

  def description(): String = {
    val exitN = exitNames.toList.zip(List("north", "south", "east", "west", "up", "down"))
    name + "\n" + desc + "\n" + "Items: " + items.map(_.name).mkString(", ") + "\n" + "Exits: " + exitN.filter(_._1 != ("")).map(_._2).mkString(", ")
  }

  def getExit(dir: Int): Option[ActorRef] = {
    exits(dir)
  }

  def getItem(itemName: String): Option[Item] = {
    items.filterNot(_.name == itemName.toLowerCase)
    val itemFound = items.find(_.name == itemName.toLowerCase)
    _items = _items.filterNot(_.name == itemName.toLowerCase)
    itemFound
  }
  def dropItem(item: Item): Unit = {
    _items = item :: _items
  }
}

object Room {
  case object PrintDescription
  case class GetItem(item: String)
  case class DropItem(item: Item)
  case class GetExit(direct: Int)
  case class LinkExits(rooms: Map[String, ActorRef])
}
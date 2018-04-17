package mud

import akka.actor.Actor
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.ActorRef

class Player(private var inv: List[Item], name: String, sock: Socket, in: BufferedReader, out: PrintStream) extends Actor {
  private var currentRoom: ActorRef = null
  
  import Player._
  def receive = {
    case ProcessInput =>
      if (in.ready()) {
        val input = in.readLine()
        processCommand(input)
      }
    case TakeExit(newRoom) =>
      newRoom match {
        case Some(nextRoom) =>
          if(currentRoom != null) currentRoom ! Room.PExit(self)
          currentRoom = nextRoom
          currentRoom ! Room.PEnter(self)
          currentRoom ! Room.PrintDescription
        case None => out.println("Error. Go away")
      }

    case AddToInventory(oitem) =>
      oitem match {
        case Some(x) => {
          addToInventory(x)
          out.println("You added the item to your inventory.")
        }
        case None => out.println("I don't see that item.")
      }
    case PrintMessage(msg) =>
      out.println(msg)

    case m => out.println("unknown message in player: " + m)
  }

  def processCommand(command: String): Unit = {
    val cmd = command.split(" ")
    if (cmd(0).toLowerCase == "look") currentRoom ! Room.PrintDescription
    if (cmd(0).toLowerCase == "inv") out.println("Inventory:" + inv.map(i => ("\n" + i.name + " - " + i.desc)).mkString)
    if ((cmd(0).toLowerCase).contains("get")) {
      currentRoom ! Room.GetItem(cmd(1))
    }
    if ((cmd(0).toLowerCase).contains("drop")) {
      getFromInventory(cmd(1)) match {
        case Some(x) => {
          val di = currentRoom ! Room.DropItem(x)
          inv = inv.filterNot(_ == x)
          di
        }
        case None => out.println("You do not have that item.")
      }
    }
    if ((cmd(0).toLowerCase) == "north" || cmd(0).toLowerCase == "south" || cmd(0).toLowerCase == "east"
      || cmd(0).toLowerCase == "west" || cmd(0).toLowerCase == "up" || cmd(0).toLowerCase == "down") {
      move(cmd(0).toLowerCase)
    }
    if(cmd(0).toLowerCase == "say"){
      val msg = command.split("say ").filter(_!="").mkString
      currentRoom ! Room.SendMessage(s"$name said $msg")
    }
    if(cmd(0).toLowerCase == "tell"){
      val msg = cmd.toList.drop(2).mkString("")
      val target = cmd(1)
      Main.playerSuper ! PlayerSupervisor.TellMessage(target, msg)
    }
    if (cmd(0).toLowerCase == "exit"){
      currentRoom ! Room.PExit(self)
      sock.close
    }
    if (command.toLowerCase == "help") out.println(
      "look - reprints the description of the current room \n" +
        "inv - list the contents of your inventory \n" +
        "get item - to get an item from the room and add it to your inventory\n" +
        "drop item - to drop an item from your inventory into the room.\n" +
        "exit - leave the game\n" +
        "help - print the available commands and what they do\n")
  }

  def getFromInventory(itemName: String): Option[Item] = {
    out.println("it's lit")
    inv.find(_.name == itemName.toLowerCase)
  }

  def addToInventory(item: Item): Unit = {
    inv = item :: inv
  }
  def inventoryListing(): String = {
    inv.mkString(" ,")
  }
  def move(dir: String): Unit = {
    val direct = dir match {
      case "north" => 0
      case "south" => 1
      case "east" => 2
      case "west" => 3
      case "up" => 4
      case "down" => 5
      case _ => -1
    }
    currentRoom ! Room.GetExit(direct)
  }
}

object Player {
  case object ProcessInput
  case class PrintMessage(msg: String)
  case class TakeExit(newRoom: Option[ActorRef])
  case class AddToInventory(item: Option[Item])
}


package mud

import akka.actor.Actor
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.ActorRef

class Player(private var inv: List[Item], name: String, sock: Socket, in: BufferedReader, out: PrintStream) extends Actor {
  private var currentRoom: ActorRef = null
  private var health: Int = 100
  private var currentWeapon: Option[Item] = None
  private var currentVictim: Option[ActorRef] = None
  def pName = name

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
          if (currentRoom != null) currentRoom ! Room.PExit(self)
          currentRoom = nextRoom
          currentRoom ! Room.PEnter(self)
          currentRoom ! Room.PrintDescription
        case None => out.println("Error. Go away")
      }

    case AddToInventory(oitem) =>
      oitem match {
        case Some(x) => {
          addToInventory(x)
          out.println("You added the item to your inventory.\n")
        }
        case None => out.println("I don't see that item.\n")
      }

    //Attack
    case CharacterInRoom(player) =>
      player match {
        case Some(x) =>
          if (x == self) out.println("You cannot kill yourself.")
          else if (!currentWeapon.isEmpty && x != self) {
            //   else{
            currentVictim = Some(x)
            Main.actSuper ! ActivityManager.ScheduleEvent(currentWeapon.get.speed, DoAttack)
            out.println("Player found.")
            //}
          } else out.println("No weapon to attack the player with.")
        case None =>
          out.println("Player not found.")
      }
    case DoAttack =>
      currentVictim match {
        case Some(x) =>
          x ! Attack(currentWeapon.get.damage, currentRoom)
          out.println("You hit them - you bastard!")
        case None => out.println("No victims to attack.")
      }

    case Attack(damage, room) =>
      if (room == currentRoom) {
        val sameRoom = true
        health -= damage
        if (currentVictim.isEmpty) {
          currentVictim = Some(sender)
          Main.actSuper ! ActivityManager.ScheduleEvent(currentWeapon.get.speed, DoAttack)
        }
        val dead = if (health <= 0) {
          currentRoom ! Room.PExit(self)
          context.stop(self)
          sock.close
          true
        } else false
        out.println("You just took " + damage + " amount of damage.")
        out.println("Health: " + health)
        sender ! AttackDone(dead, sameRoom)
      } else sender ! AttackDone(false, false)

    //goes back to attacker
    case AttackDone(victimDead, sameRoom) =>
      if (victimDead && sameRoom) {
        out.println("Your victim is dead.")
        currentVictim = None
      } else if (!victimDead && !sameRoom) {
        out.println("They seem to be gone.")
        currentVictim = None
      } else {
        Main.actSuper ! ActivityManager.ScheduleEvent(currentWeapon.get.speed, DoAttack)
      }

    case PrintMessage(msg) =>
      out.println(msg)

    case m => println("unknown message in player: " + m)
  }

  def processCommand(command: String): Unit = {
    val cmd = command.split(" ")
    if (cmd(0).toLowerCase == "look") currentRoom ! Room.PrintDescription
    //items
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

    /* Every time a player equip a weapon, other players are warned. This warning is helpful because if the player with a weapon decides to 
  * initate combat, the attacked player must have a weapon equipped in order to attack back.
  */
    if (cmd(0).toLowerCase == "equip") {
      if (currentWeapon.isEmpty) {
        getFromInventory(cmd(1)) match {
          case Some(x) => {
            currentWeapon = Some(x)
            inv = inv.filterNot(_ == x)
            out.println(x.name + " is equipped.")
            currentRoom ! Room.SendMessage("\nWatch out! " + pName + " is equipped with a " + x.name)
          }
          case None => out.println("You do not have that item.\n")
        }
      } else out.println("You already have a weapon - " + currentWeapon.map(_.name).mkString + ".")
    }
    if (command.toLowerCase == "unequip") {
      if (!currentWeapon.isEmpty) {
        addToInventory(currentWeapon.get)
        out.println(currentWeapon.get.name + " is unequipped.\n")
        currentWeapon = None
      } else out.println("There is nothing to unequip.")
    }
    //movement
    if ((cmd(0).toLowerCase) == "north" || cmd(0).toLowerCase == "south" || cmd(0).toLowerCase == "east"
      || cmd(0).toLowerCase == "west" || cmd(0).toLowerCase == "up" || cmd(0).toLowerCase == "down") {
      if (currentVictim.isEmpty) move(cmd(0).toLowerCase)
      else out.println("You are in combat. You cannot move.")
    }
    if (cmd(0).toLowerCase == "flee") {
      if (currentVictim.isEmpty) out.println("You cannot flee.")
      else currentRoom ! Room.GetExit(util.Random.nextInt(6))
    }
    if (cmd(0).toLowerCase == "exit") {
      currentRoom ! Room.PExit(self)
      context.stop(self)
      sock.close
    }
    if (cmd(0).toLowerCase == "shortestpath") Main.roomSuper ! RoomSupervisor.ShortPath(self, currentRoom.path.name, cmd(1).toLowerCase)

//communication between players
    if (cmd(0).toLowerCase == "say") {
      val msg = command.split("say ").filter(_ != "").mkString
      currentRoom ! Room.SendMessage(s"$name said $msg")
    }
    if (cmd(0).toLowerCase == "tell") {
     // val msg = cmd.toList.drop(2).mkString("")
      val msg = command.split("tell ").filter(_ != "").mkString
      val target = cmd(1)
      Main.playerSuper ! PlayerSupervisor.TellMessage(target, msg)
    }

    if (cmd(0).toLowerCase == "kill") {
      if (currentVictim.isEmpty) currentRoom ! Room.GetCharacter(cmd(1))
      else out.println("You're already attacking someone.")
    }
    if (command.toLowerCase == "health") {
      out.println("Health: " + health)
    }

    if (command.toLowerCase == "help") out.println(
      "look - reprints the description of the current room \n" +
        "inv - list the contents of your inventory \n" +
        "get item - to get an item from the room and add it to your inventory\n" +
        "drop item - to drop an item from your inventory into the room.\n" +
        "exit - leave the game\n" +
        "help - print the available commands and what they do\n" +
        "equip item - get the item that you want to use for attacking\n" +
        "unequip - stop using current equipped item as attack weapon \n" +
        "health - displays current health" +
        "kill name - initiates combat with another player in the game" +
        "shortestPath roomkey - lists directions of the shortest path to the requested room")
  }

  def getFromInventory(itemName: String): Option[Item] = {
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
  case class CharacterInRoom(player: Option[ActorRef])
  case object DoAttack
  case class Attack(damage: Int, room: ActorRef)
  case class AttackDone(victimDead: Boolean, sameRoom: Boolean)
}


package mud

import java.io.PrintStream
import akka.actor.Props
import java.io.BufferedReader
import akka.actor.Actor
import java.net.Socket

class PlayerSupervisor extends Actor {
  import PlayerSupervisor._
  def receive = {
    case ProcessAllInput =>
      for (child <- context.children) child ! Player.ProcessInput
    case NewPlayer(currentRoom, inv, name, sock, in, out) =>
      val newPlayer = context.actorOf(Props(new Player(inv, name, sock, in, out)), name)
      Main.roomSuper ! RoomSupervisor.Location(newPlayer, currentRoom)
    case SendMessage(msg) =>
      for (child <- context.children) child ! Player.PrintMessage(msg)
    case m => println(s"Bad message in PlayerSuper: $m")
  }
}

object PlayerSupervisor {
  case object ProcessAllInput
  case class NewPlayer(currentRoom: String, inv: List[Item], name: String, sock: Socket, in: BufferedReader, out: PrintStream)
  case class SendMessage(msg: String)
  case class FindItem(item: String)
}
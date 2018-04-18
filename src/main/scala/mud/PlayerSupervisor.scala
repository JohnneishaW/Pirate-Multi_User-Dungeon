package mud

import java.io.BufferedReader
import java.io.PrintStream
import java.net.Socket

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

class PlayerSupervisor extends Actor {
  import PlayerSupervisor._
  
  def receive = {
    case ProcessAllInput =>
      for (child <- context.children) child ! Player.ProcessInput
    case NewPlayer(currentRoom, inv, name, sock, in, out) =>
      val newPlayer = context.actorOf(Props(new Player(inv, name, sock, in, out)), name)
      Main.roomSuper ! RoomSupervisor.Location(newPlayer, currentRoom)
     case  TellMessage(target, msg) =>
       for(child<- context.children) if (child.path.name == target) child ! Player.PrintMessage(s"ghosty said $msg")
     
    case m => println(s"Bad message in PlayerSuper: $m")
  }
}

object PlayerSupervisor {
  case object ProcessAllInput
  case class TellMessage(target:String, msg: String)
  case class NewPlayer(currentRoom: String, inv: List[Item], name: String, sock: Socket, in: BufferedReader, out: PrintStream)
}
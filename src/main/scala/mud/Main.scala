package mud

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import java.net.ServerSocket
import scala.concurrent.duration._

import scala.concurrent.Future

import akka.actor.ActorSystem
import akka.actor.Props

object Main extends App {
  
  /*
		val startingInv:List[Item]=Nil
	  val player = new Player("gal", startingInv)
		
		var input = readLine.toLowerCase
		while(input != "exit"){
		  player.processCommand(input)
		  input=readLine.toLowerCase
	  }*/

  val system = ActorSystem("JMud")
  val playerSuper = system.actorOf(Props[PlayerSupervisor], "pSuper")
  val roomSuper = system.actorOf(Props[RoomSupervisor], "rSuper")
  val actSuper =  system.actorOf(Props[ActivityManager], "aSuper")
  val npcSuper = system.actorOf(Props[NPCSupervisor], "nSuper")
  val startingInv: List[Item] = Nil

  implicit val ec = system.dispatcher
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerSuper, PlayerSupervisor.ProcessAllInput)
  system.scheduler.schedule(0.seconds, 0.1.seconds, actSuper, ActivityManager.DoEvents)
  
  val ss = new ServerSocket(4004)
  while (true) {
    val sock = ss.accept()
    Future {
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      out.println("You connected! Welcome to my MUD. \n")
      out.println("What is your name?")
      val name = in.readLine()
      out.println("\n")
      playerSuper ! PlayerSupervisor.NewPlayer("gal", startingInv, name,  sock, in, out)
    }
  }
}


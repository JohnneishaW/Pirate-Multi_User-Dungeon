package mud

import scala.io.StdIn._

object Main {
	def main(args: Array[String]): Unit = {
		println("Welcome to my MUD.")
		val startingInv:List[Item]=Nil
	  val player = new Player(0, startingInv)
		
		var input = readLine.toLowerCase
		while(input != "exit"){
		  player.processCommand(input)
		  input=readLine.toLowerCase
	}
}
}

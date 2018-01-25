package mud

import scala.io.StdIn._

object Main {
	def main(args: Array[String]): Unit = {
		println("Welcome to my MUD.")
		
		val input = readLine
		while(input != "exit"){
		  val r = new Room
		  val i = new Item
	    val player = new Player(r, i)
	}
}

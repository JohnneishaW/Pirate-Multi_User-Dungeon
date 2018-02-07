package mud

class Player(private var currentRoom: Int, private var inv:List[Item]) {
  def processCommand(command: String): Unit = {
    val cmd= command.split(" ")
    if (cmd(0).toLowerCase == "look") println(Room.rooms(currentRoom).description())
    if (cmd(0).toLowerCase == "inv") println(inventoryListing())
    if ((cmd(0).toLowerCase).contains("get")){
      getFromInventory(cmd(1))
    }
    if ((cmd(0).toLowerCase).contains("drop")) {
         getFromInventory(cmd(1)) match {
            case Some(x) => Room.rooms(currentRoom).dropItem(x)
            case None => 
          }
          
        inv.filterNot(_==cmd(1))
    }
    if(command.toLowerCase == "exit") "exit"
    if (command.toLowerCase == "help") println(
      "look - reprints the description of the current room \n" +
      "inv/inventory - list the contents of your inventory \n" +
      "get item - to get an item from the room and add it to your inventory\n" +
      "drop item - to drop an item from your inventory into the room.\n" +
      "exit - leave the game\n" +
      "help - print the available commands and what they do\n")
  }
  
 
  def getFromInventory(itemName: String): Option[Item] = {
    ???
//    inv.find(_==cmd(1)) match {
//      case Some(x) => x
//      case None => 
//    }
  }
 
  def addToInventory(item: Item): Unit = {
    inv=item::inv
  }
  def inventoryListing(): String = {
    inv.mkString(" ,")
  }
  def move(dir: String): Unit = {
    //want to change rooms using array[int]
    val direct = dir match{
      case "north" => 0
      case "south" => 1
      case "east" => 2
      case "west" => 3
      case "up" => 4
      case "down" => 5
      case _ => -1
    }
    Room.rooms(currentRoom).getExit(direct) match{
      case Some(nextRoom) =>
        currentRoom = nextRoom
        println(Room.rooms(nextRoom).description)
      case None => println("Error. Go away")
    }
  }

}


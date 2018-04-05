package mud

class Player(private var currentRoom: Int, private var inv: List[Item]) {
  def processCommand(command: String): Unit = {
    val cmd = command.split(" ")
    
    
    if (cmd(0).toLowerCase == "look") println(Room.rooms(currentRoom).description())
    if (cmd(0).toLowerCase == "inv") println("Inventory:\n" + inv.map(_.name).mkString + " - " + inv.map(_.desc).mkString)
    if ((cmd(0).toLowerCase).contains("get")) {
      Room.rooms(currentRoom).getItem(cmd(1)) match {
        case Some(x) => {
          addToInventory(x)
          println("I added the item to my inventory.")
        }
        case None =>
      }
    }
    if ((cmd(0).toLowerCase).contains("drop")) {
       getFromInventory(cmd(1)) match {
        case Some(x) => Room.rooms(currentRoom).dropItem(x)
        case None =>
      }
    }
    if((cmd(0).toLowerCase) == "north"||cmd(0).toLowerCase == "south"||cmd(0).toLowerCase == "east"
        ||cmd(0).toLowerCase == "west"||cmd(0).toLowerCase == "up"||cmd(0).toLowerCase == "down"){
      move(cmd(0).toLowerCase)
    }
    
    if (command.toLowerCase == "exit") "exit"
    if (command.toLowerCase == "help") println(
      "look - reprints the description of the current room \n" +
        "inv/inventory - list the contents of your inventory \n" +
        "get item - to get an item from the room and add it to your inventory\n" +
        "drop item - to drop an item from your inventory into the room.\n" +
        "exit - leave the game\n" +
        "help - print the available commands and what they do\n")
  }

  def getFromInventory(itemName: String): Option[Item] = {
    println("it's lit")
    inv.find(_.name == itemName.toLowerCase)

    /*match {
      case Some(x) => Option(x)
     case None => 
   }*/
  }

  def addToInventory(item: Item): Unit = {
    inv = item :: inv
  }
  def inventoryListing(): String = {
    inv.mkString(" ,")
  }
  def move(dir: String): Unit = {
    //want to change rooms using array[int]
    val direct = dir match {
      case "north" => 0
      case "south" => 1
      case "east" => 2
      case "west" => 3
      case "up" => 4
      case "down" => 5
      case _ => -1
    }
    Room.rooms(currentRoom).getExit(direct) match {
      case Some(nextRoom) =>
        currentRoom = nextRoom
        println(Room.rooms(nextRoom).description)
      case None => println("Error. Go away")
    }
  }

}


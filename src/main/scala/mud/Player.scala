package mud

class Player(private var currentRoom: Room, val inv:List[Item]) {
  def processCommand(command: String): Unit = {
    val cmd= command.split(" ")
    if (cmd(0).toLowerCase == "look") println(currentRoom.description())
    if (cmd(0).toLowerCase == "inv") println(inventoryListing())
    if ((cmd(0).toLowerCase).contains("get")){
      getFromInventory(cmd(1))
    }
    if ((cmd(0).toLowerCase).contains("drop")) {
         getFromInventory(cmd(1)) match {
            case Some(x) => currentRoom.dropItem(x)
            case None => 
          }
          
        inv.filterNot(_==cmd(1))
    }
    if(command.toLowerCase == "exit") //TODO: exit
    if (command.toLowerCase == "help") println(
      "look - reprints the description of the current room \n" +
      "inv/inventory - list the contents of your inventory \n" +
      "get item - to get an item from the room and add it to your inventory\n" +
      "drop item - to drop an item from your inventory into the room.\n" +
      "exit - leave the game\n" +
      "help - print the available commands and what they do\n")
  }
  
  def getFromInventory(itemName: String): Option[Item] = {
    inv.find(_==cmd(1)) match {
      case Some(x) => x
      case None => 
    }
  }
  def addToInventory(item: Item): Unit = {
    item::inv
  }
  def inventoryListing(): String = {
    inv.foreach(i => println(i))
  }
  def move(dir: String): Unit = {
    val ex = currentRoom.getExits()
    if(ex==dir) currentRoom == Room.rooms(
  }

}


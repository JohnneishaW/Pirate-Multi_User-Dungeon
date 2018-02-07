package mud

//change exits from ints to strings
class Room(private val name:String, private val desc:String, exits:Array[Int], private var _items: List[Item]){
  def items = _items 
  
  def description(): String = {
    name + "\n" + desc + "\n" + items + "\n" + exits.map(_.toString)
  }
  def getExit(dir: Int): Option[Int] = {
    exits.find(_==dir)
  }
  def getItem(itemName: String): Option[Item] = {
    items.find(_==itemName.toLowerCase)
  }
  def dropItem(item: Item): Unit = {
    item::items
   }
}


object Room {
  val rooms = readRooms()

  //change to Map[String,Room]
  def readRooms(): Array[Room] = {
    val xmlData = xml.XML.loadFile("RoomData.xml")
    (xmlData \ "room").map(n => {
      val name = (n \ "@name").text
      val desc = (n \ "description").text
      val exits = (n \ "exits").text.split(",").map(_.toInt)
      val items = (n \ "item").map(in =>
        Item((in \ "name").text, in.text)).toList
      val id = (n \ "@id").text
      new Room(name, desc, exits, items)
    }).toArray
    //change to map
}
}
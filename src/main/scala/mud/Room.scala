package mud

//change exits from ints to strings
class Room(private val name:String, private val desc:String, val exits:Array[String], private var _items: List[Item]){
  def items = _items 
  
  def description(): String = {
    val exitNames = exits.toList.zip(List("north","south","east","west","up", "down"))
    name + "\n" + desc + "\n" +"Items: " + items.map(_.name).mkString(", ") +"\n" + "Exits: " + exitNames.filter(_._1!=("")).map(_._2).mkString(", ")
    
   }

  def getExit(dir: Int): Option[String] = {
    exits(dir)match{
      case "" => None
      case x => Some(x)
    }
  }
 
  def getItem(itemName: String): Option[Item] = {
   items.filterNot(_.name==itemName.toLowerCase)
   val itemFound = items.find(_.name==itemName.toLowerCase)
   _items = _items.filterNot(_.name==itemName.toLowerCase)
   itemFound
  }
  def dropItem(item: Item): Unit = {
    _items = item::_items
   }
}


object Room {
  val rooms = readRooms()

  def readRooms()= {
    val xmlData = xml.XML.loadFile("RoomData.xml")
    (xmlData \ "room").map(n => {
      val name = (n \ "@name").text
      val desc = (n \ "description").text
      val exits = (n \ "exits").text.split(",").map(_.trim)
      val items = (n \ "item").map(in =>
        Item((in \ "@name").text, in.text)).toList
      val id = (n \ "@id").text
      val key= (n \ "@key").text
      (key, new Room(name, desc, exits, items))
     //new Room(name, desc, exits, items)
    }).toMap[String,Room]
    //.toArray
  }
}
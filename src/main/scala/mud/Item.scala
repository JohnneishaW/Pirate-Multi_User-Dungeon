package mud

case class Item(val name: String, val damage: Int, val speed:Int, val desc: String) {
  
}

object Item{
  def apply(n: xml.Node): Item = {
    val name = (n \ "@name").text
    val damage = (n \ "@damage").text.toInt
    val speed = (n \ "@speed").text.toInt
    val desc = (n.text)
    Item(name, damage,speed,desc)
  }
}
package mud

class MutableDLL[A] {
  import MutableDLL.Node

  private var default: A = _
  private var end: Node[A] = new Node[A](null, default, null)
  end.prev = end
  end.next = end
  private var _length = 0

  def apply(index: Int): A = {
    require(index >= 0 && index < _length)
    var rover = end.next
    for (_ <- 0 until index) rover = rover.next
    rover.data
  }

  def update(index: Int, a: A): Unit = {
    require(index >= 0 && index < _length)
    var rover = end.next
    for (_ <- 0 until index) rover = rover.next
    rover.data = a
  }

  def add(index: Int, a: A): Unit = {
    require(index >= 0 && index <= _length)
    var rover = end
    for (_ <- 0 until index - 1) rover = rover.next
    val n = new Node[A](rover, a, rover.next)
    rover.next.prev = n
    rover.next = n
    _length += 1
  }

  def remove(index: Int): A = {
    require(index >= 0 && index <= _length)
    _length -= 1
    var rover = end.next
    for (_ <- 0 until index - 1) rover = rover.next
    rover.prev.next = rover.next
    rover.next.prev = rover.prev
    rover.data
  }

  def length: Int = _length

  def +=(a: A): MutableDLL[A] = {
    val n = new Node[A](end.prev, a, end)
    //order matters for these two
    end.prev.next = n
    end.prev = n
    _length += 1
    this
  }

  def -=(a: A): MutableDLL[A] = {
    val n = new Node[A](end.prev, a, end)
    end.prev.next = end.next
    end.next.prev = end.prev
    _length -= 1
    this
  }

  def filter(pred: A => Boolean): MutableDLL[A] = {
    val ret = new MutableDLL[A]()
    var rover = end.next
    while (rover != end) {
      if (pred(rover.data)) {
        ret += rover.data
      }
      rover = rover.next
    }
    ret
  }
  def filterNot(pred: A => Boolean): MutableDLL[A] = {
    val ret = new MutableDLL[A]()
    var rover = end.next
    while (rover != end) {
      if (!pred(rover.data)) {
        ret += rover.data
      }
      rover = rover.next
    }
    ret
  }
  def find(pred: A => Boolean): Option[A] = {
    var ret: Option[A] = None
    var rover = end.next
    while (rover != end && ret.isEmpty) {
      if (pred(rover.data)) {
        ret = Some(rover.data)
      }
      rover = rover.next
    }
    ret
  }
  def map[B](f: A => B): MutableDLL[B] = {
    val ret = new MutableDLL[B]()
    var rover = end.next
    while (rover != end) {
      ret += f(rover.data)
      rover = rover.next
    }
    ret
  }

  def mkString(d:String): String = {
    var ret = ""
    var rover = end.next
    while (rover != end ) {
    if(ret.nonEmpty) ret +=d
      ret += rover.data 
      rover = rover.next
    }
    ret
  }

}

object MutableDLL {
  class Node[A](var prev: Node[A], var data: A, var next: Node[A])
  def apply[A](as: A*): MutableDLL[A] = {
    val ret = new MutableDLL[A]()
    for(a <- as) ret +=a
    ret
  }
}

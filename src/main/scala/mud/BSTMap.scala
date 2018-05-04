package mud

import collection.mutable


class BSTMap[K, V](comp: (K, K) => Int) extends collection.mutable.Map[K, V] {
  import BSTMap._
  private var root: Node[K, V] = null

  def get(key: K): Option[V] = {
    var rover = root
    while(rover != null && comp(key, rover.kv._1) != 0) {
      if(comp(key, rover.kv._1) < 0) {
        rover = rover.left
      } else {
        rover = rover.right
      }
    }
    if(rover==null) None else Some(rover.kv._2)
  }

  def iterator: Iterator[(K, V)] = ???

  def +=(kv: (K, V)) = {
    def adder(n: Node[K, V]): Node[K, V] = {
      if (n == null) {
        new Node(kv, null, null)
      } else {
        val c = comp(kv._1, n.kv._1)
        if (c < 0) {
          n.left = adder(n.left)
        } else if (c > 0) {
          n.right = adder(n.right)
        } else {
          n.kv = kv
        }
        n
      }
    }
    root = adder(root)
    this
  }

  def -=(key: K) = {
    this
  }

  def preorder(visit: ((K, V)) => Unit): Unit = {
    def helper(n: Node[K, V]): Unit = {
      if (n != null) {
        visit(n.kv)
        helper(n.left)
        helper(n.right)
      }
    }
    helper(root)
  }

  def postorder(visit: ((K, V)) => Unit): Unit = {
    def helper(n: Node[K, V]): Unit = {
      if (n != null) {
        helper(n.left)
        helper(n.right)
        visit(n.kv)
      }
    }
    helper(root)
  }

  def inorder(visit: ((K, V)) => Unit): Unit = {
    def helper(n: Node[K, V]): Unit = {
      if (n != null) {
        helper(n.left)
        visit(n.kv)
        helper(n.right)
      }
    }
    helper(root)
  }
}

object BSTMap extends App {
  private class Node[K, V](var kv: (K, V), var left: Node[K, V], var right: Node[K, V])
}
package porcEpic

import scala.collection.mutable.StringBuilder

type EntryLinkedList[S, T] = DoubleLinkedList[EntryNode[S, T]]

object EntryLinkedList {
  def apply[S, T](entries: List[Entry[S, T]]): EntryLinkedList[S, T] = {
    var root: DoubleLinkedList[EntryNode[S, T]] = null
    val matches = collection.mutable.Map.empty[Int, EntryLinkedList[S, T]]

    entries.reverse.foreach{ elem => 
      val entry =
        elem match {
          case r: Entry.Return[_, _] =>
            val entry = 
              new DoubleLinkedList[EntryNode[S, T]](
                EntryNode.Return[S, T](r.id, r.value)
              )
            matches(r.id) = entry
            entry

          case c: Entry.Call[_, _] =>
            new DoubleLinkedList[EntryNode[S, T]](
              EntryNode.Call(c.id, c.value, matches.getOrElse(c.id, throw new Exception("cannot find match")))
            )
        }

      entry.insertBefore(root)
      root = entry
    }

    root
  }
}

object DoubleLinkedList {
  def apply[T](xs: T*): DoubleLinkedList[T] = {
    xs.reverse.foldLeft(null: DoubleLinkedList[T])((acc, x) =>
      (new DoubleLinkedList(x)).insertBefore(acc)
    )
  }
}

class DoubleLinkedList[T](
  val elem: T, 
  var prev: DoubleLinkedList[T] = null,
  var next: DoubleLinkedList[T] = null
) {

  def length: Int = {
    var l = 1
    var n = this
    while (n.next != null) {
      n = n.next
      l += 1
    }
    l
  }

  def insertBefore(mark: DoubleLinkedList[T]): this.type = {
    if (mark != null) {
      val beforeMark = mark.prev
      mark.prev = this
      this.next = mark
      if (beforeMark != null) {
        this.prev = beforeMark
        beforeMark.next = this
      }
    }
    this
  }

  override def toString: String = {
    val builder = new StringBuilder("DoubleLinkedList(\n")

    if (prev != null) {
      builder ++= "  ...,\n"
    } else {
    }

    def show(that: DoubleLinkedList[T]): Unit = {
      if (that != null) {
        builder ++= s"  ${that.elem.toString},\n"
      }
    }

    var current = this
    show(current)
    while (current.next != null) {
      current = current.next
      show(current)
    }
    builder ++= ")"
    builder.toString
  }
}

/**
 * @param matches:  if it's a call it points to the return entry
 */

sealed trait EntryNode[S, T] {
  val id: Int
}
object EntryNode {
  case class Call[S, T](id: Int, value: T, matches: EntryLinkedList[S, T]) extends EntryNode[S, T] {
    override def toString: String = {
      s"Call($id, $value)"
    }
  }
  case class Return[S, T](id: Int, value: S) extends EntryNode[S, T] {
    override def toString: String = {
      s"Return($id, $value)"
    }
  }
}

extension [S, T](list: DoubleLinkedList[EntryNode[S, T]]) {

  def lift(): Unit = {
    list.elem match {
      case c: EntryNode.Call[_, _] =>
        import list._
        prev.next = next
        next.prev = prev
        c.matches.prev.next = c.matches.next
        if (c.matches.next != null) {
          c.matches.next.prev = c.matches.prev
        }

      case _ => throw new Exception("cannot lift on Return Entry")
    }

  }

  def unlift(): Unit = {
    list.elem match {
      case c: EntryNode.Call[_, _] =>
        import list._

        c.matches.prev.next = c.matches
        if (c.matches.next != null) {
          c.matches.next.prev = c.matches
        }
        prev.next = list
        next.prev = list

      case _ => throw new Exception("cannot unlift on Return Entry")
    }
  }
}

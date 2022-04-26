package porcEpic

import scala.collection.mutable.StringBuilder

type EntryLinkedList[I, O] = DoubleLinkedList[EntryNode[I, O]]

object EntryLinkedList {
  def apply[S, I, O](entries: List[Entry[I, O]]): EntryLinkedList[I, O] = {
    var root: DoubleLinkedList[EntryNode[I, O]] = null
    val matches = collection.mutable.Map.empty[OperationId, EntryLinkedList[I, O]]

    entries.reverse.foreach{ elem =>
      val entry =
        elem match {
          case r: Entry.Return[_, _] =>
            val entry = 
              new DoubleLinkedList[EntryNode[I, O]](
                EntryNode.Return[I, O](r.id, r.output)
              )
            matches(r.id) = entry
            entry

          case c: Entry.Call[_, _] =>
            new DoubleLinkedList[EntryNode[I, O]](
              EntryNode.Call(c.id, c.input, matches.getOrElse(c.id, throw new Exception("cannot find match")))
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

sealed trait EntryNode[I, O] {
  val id: OperationId
}
object EntryNode {
  case class Call[I, O](id: OperationId, input: I, matches: EntryLinkedList[I, O]) extends EntryNode[I, O] {
    override def toString: String = {
      s"Call($id, $input)"
    }
  }
  case class Return[I, O](id: OperationId, output: O) extends EntryNode[I, O] {
    override def toString: String = {
      s"Return($id, $output)"
    }
  }
}

extension [I, O](list: DoubleLinkedList[EntryNode[I, O]]) {

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

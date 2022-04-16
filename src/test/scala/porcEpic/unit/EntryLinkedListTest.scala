package porcEpic
package unit

import TestData.history

import org.scalatest.funsuite.AnyFunSuite

class EntryLinkedListTest extends AnyFunSuite {

  val entries = Entry.fromOperations(history)

  test("toString") {

    val obtained = DoubleLinkedList(1, 2, 3).toString
    val expected = 
      """|DoubleLinkedList(
         |  1,
         |  2,
         |  3,
         |)""".stripMargin
    
    assert(obtained == expected)
  }

  test("EntryLinkedList") {
    val obtained = EntryLinkedList(entries).toString
    val expected = 
      """|DoubleLinkedList(
         |  Call(0, (W,0)),
         |  Return(0, 0),
         |  Call(1, (W,1)),
         |  Call(2, (R,1)),
         |  Call(3, (R,0)),
         |  Return(3, 1),
         |  Return(2, 1),
         |  Return(1, 1),
         |)""".stripMargin

    assert(obtained == expected)
  }

  test("length"){
    assert(EntryLinkedList(entries).length == 8)
  }
}

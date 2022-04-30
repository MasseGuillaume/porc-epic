package porcEpic
package unit

import TestData.history

class EntryLinkedListTest extends munit.FunSuite {

  val entries = Entry.fromOperations(history)

  test("toString") {

    val obtained = DoubleLinkedList(1, 2, 3).toString
    val expected = 
      """|DoubleLinkedList(
         |  1,
         |  2,
         |  3,
         |)""".stripMargin
    
    assertEquals(obtained, expected)
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

    assertEquals(obtained, expected)
  }

  test("length"){
    assertEquals(EntryLinkedList(entries).length, 8)
  }
}

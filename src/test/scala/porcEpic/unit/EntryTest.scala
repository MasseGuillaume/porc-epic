package porcEpic
package unit

import TestData.{history, OperationKind}
import OperationKind._

import org.scalatest.funsuite.AnyFunSuite

class EntryTest extends AnyFunSuite {
  
  test("fromOperations"){
    val obtained = Entry.fromOperations(history)
    val expected = 
      List(
        Entry.Call   ((W, 0), Time(0), OperationId(0), ClientId(0)),
        Entry.Return (0     , Time(1), OperationId(0), ClientId(0)),
        Entry.Call   ((W, 1), Time(2), OperationId(1), ClientId(1)),
        Entry.Call   ((R, 1), Time(3), OperationId(2), ClientId(2)),
        Entry.Call   ((R, 0), Time(4), OperationId(3), ClientId(3)),
        Entry.Return (1     , Time(5), OperationId(3), ClientId(3)),
        Entry.Return (1     , Time(6), OperationId(2), ClientId(2)),
        Entry.Return (1     , Time(7), OperationId(1), ClientId(1))
      )

    assert(obtained == expected)
  }
}

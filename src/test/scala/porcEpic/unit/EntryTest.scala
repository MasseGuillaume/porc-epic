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
        Entry.Call   ( value = (W, 0), time = Time(0), id = OperationId(0), clientId = ClientId(0)),
        Entry.Return ( value = 0     , time = Time(1), id = OperationId(0), clientId = ClientId(0)),
        Entry.Call   ( value = (W, 1), time = Time(2), id = OperationId(1), clientId = ClientId(1)),
        Entry.Call   ( value = (R, 1), time = Time(3), id = OperationId(2), clientId = ClientId(2)),
        Entry.Call   ( value = (R, 0), time = Time(4), id = OperationId(3), clientId = ClientId(3)),
        Entry.Return ( value = 1     , time = Time(5), id = OperationId(3), clientId = ClientId(3)),
        Entry.Return ( value = 1     , time = Time(6), id = OperationId(2), clientId = ClientId(2)),
        Entry.Return ( value = 1     , time = Time(7), id = OperationId(1), clientId = ClientId(1))
      )

    assert(obtained == expected)
  }
}

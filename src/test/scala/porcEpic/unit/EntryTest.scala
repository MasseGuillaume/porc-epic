package porcEpic
package unit

import porcEpic.{fromLong => t}
import TestData.{history, OperationKind}
import OperationKind._

import org.scalatest.funsuite.AnyFunSuite

class EntryTest extends AnyFunSuite {
  
  test("fromOperations"){
    val obtained = Entry.fromOperations(history)
    val expected = 
      List(
        Entry.Call   ( value = (W, 0), time = t(0), id = opid(0), clientId = cid(0)),
        Entry.Return ( value = 0     , time = t(1), id = opid(0), clientId = cid(0)),
        Entry.Call   ( value = (W, 1), time = t(2), id = opid(1), clientId = cid(1)),
        Entry.Call   ( value = (R, 1), time = t(3), id = opid(2), clientId = cid(2)),
        Entry.Call   ( value = (R, 0), time = t(4), id = opid(3), clientId = cid(3)),
        Entry.Return ( value = 1     , time = t(5), id = opid(3), clientId = cid(3)),
        Entry.Return ( value = 1     , time = t(6), id = opid(2), clientId = cid(2)),
        Entry.Return ( value = 1     , time = t(7), id = opid(1), clientId = cid(1))
      )

    assert(obtained == expected)
  }
}

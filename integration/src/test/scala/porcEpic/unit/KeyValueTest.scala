package porcEpic
package unit

class KeyValueTest extends munit.FunSuite {

  import specification.KeyValue._
  import Input._
  
  test("simple put") {

    val history = List[Entry[Input, Output]](
      Entry.Call(Put("k", State("1")), Time(0) , OperationId(0), ClientId(0)),
      Entry.Return(Output("0"),        Time(10), OperationId(0), ClientId(0)),
      Entry.Call(Get("k"),             Time(2) , OperationId(2), ClientId(0)),
      Entry.Return(Output("1"),        Time(7) , OperationId(2), ClientId(0)),
      Entry.Call(Get("k"),             Time(3) , OperationId(3), ClientId(0)),
      Entry.Return(Output("0"),        Time(7) , OperationId(3), ClientId(0)),
    )

    val (result, _) = specification.checkEntries(history)

    assertEquals(result, CheckResult.Illegal)
  }
}
package porcEpic
package unit

class RegisterTest extends munit.FunSuite {

  import specification.Register._
  import Input._

  test("linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(OperationId(1), ClientId(0), Put(State(1)), Output(0), invocation = Time(0), response = Time(10)),
      Operation(OperationId(2), ClientId(1), Get,           Output(1), invocation = Time(2), response = Time(7)),
      Operation(OperationId(3), ClientId(2), Get,           Output(0), invocation = Time(3), response = Time(7)),
    )
    val (result, info) = model.checkOperations(ops)
    assertEquals(info.get.partialLinearizations, List(List(List(OperationId(2), OperationId(0), OperationId(1)))))
    assertEquals(result, CheckResult.Ok)
  }

  test("not-linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(OperationId(1), ClientId(0), Put(State(1)), Output(0), invocation = Time(0), response = Time(10)),
      Operation(OperationId(2), ClientId(1), Get,           Output(1), invocation = Time(1), response = Time( 4)),
      Operation(OperationId(3), ClientId(2), Get,           Output(0), invocation = Time(5), response = Time(10)),
    )
    val (result, info) = model.checkOperations(ops)
    assertEquals(info.get.partialLinearizations, List(List(List(OperationId(0), OperationId(1)))))
    assertEquals(result, CheckResult.Illegal)
  }
}

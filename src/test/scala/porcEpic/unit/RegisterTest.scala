package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

class RegisterTest extends AnyFunSuite {

  import specification.Register._
  import Input._

  test("linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = OperationId(1), clientId = ClientId(0), input = Put(state(1)), invocation = Time(0), output = output(0), response = Time(10)),
      Operation(id = OperationId(2), clientId = ClientId(1), input = Get,           invocation = Time(2), output = output(1), response = Time(7)),
      Operation(id = OperationId(3), clientId = ClientId(2), input = Get,           invocation = Time(3), output = output(0), response = Time(7)),
    )
    val (result, info) = model.checkOperations(ops)
    assert(info.get.partialLinearizations == List(List(List(2, 0, 1))))
    assert(result == CheckResult.Ok)
  }

  test("not-linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = OperationId(1), clientId = ClientId(0), input = Put(state(1)), invocation = Time(0), output = output(0), response = Time(10)),
      Operation(id = OperationId(2), clientId = ClientId(1), input = Get,           invocation = Time(1), output = output(1), response = Time( 4)),
      Operation(id = OperationId(3), clientId = ClientId(2), input = Get,           invocation = Time(5), output = output(0), response = Time(10)),
    )
    val (result, info) = model.checkOperations(ops)
    assert(info.get.partialLinearizations == List(List(List(0, 1))))
    assert(result == CheckResult.Illegal)
  }
}

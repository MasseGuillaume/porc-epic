package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class RegisterTest extends AnyFunSuite {

  import specification.Register._
  import Input._

  test("linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(clientId = cid(0), input = Put(state(1)), invocation = t(0), output = output(0), response = t(10)),
      Operation(clientId = cid(1), input = Get,           invocation = t(2), output = output(1), response = t(7)),
      Operation(clientId = cid(2), input = Get,           invocation = t(3), output = output(0), response = t(7)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(info.partialLinearizations == List(List(List(2, 0, 1))))
    assert(result == CheckResult.Ok)
  }

  test("not-linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(clientId = cid(0), input = Put(state(1)), invocation = t(0), output = output(0), response = t(10)),
      Operation(clientId = cid(1), input = Get,           invocation = t(1), output = output(1), response = t( 4)),
      Operation(clientId = cid(2), input = Get,           invocation = t(5), output = output(0), response = t(10)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(info.partialLinearizations == List(List(List(0, 1))))
    assert(result == CheckResult.Illegal)
  }
}

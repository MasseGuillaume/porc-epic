package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

class SetTest extends AnyFunSuite {

  import specification.SetModel._
  
  import Input._
  import porcEpic.{OperationId => opid, ClientId => cid, Time => t}

  test("set 01") {
    val ops = List[Operation[Input, Output]](
      Operation(opid(1), cid(0), Write(State(100)), Output.Write, invocation = t(0), response = t(5)),
      Operation(opid(2), cid(1), Write(State(0)),  Output.Write,  invocation = t(1),  response = t(4)),
      Operation(opid(3), cid(2), Read, Output.Read(State(100)), invocation = t(2), response = t(3)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 02") {
    val ops = List[Operation[Input, Output]](
      Operation(opid(1), cid(0), Write(State(100)), Output.Write, invocation = t(0), response = t(5)),
      Operation(opid(2), cid(1), Write(State(110)), Output.Write,  invocation = t(1),  response = t(4)),
      Operation(opid(3), cid(2), Read, Output.Read(State(100, 110)), invocation = t(2), response = t(3)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 03") {
    val ops = List[Operation[Input, Output]](
      Operation(opid(1), cid(0), Write(State(100)), Output.Write, invocation = t(0), response = t(5)),
      Operation(opid(2), cid(1), Write(State(110)), Output.Write,  invocation = t(1),  response = t(4)),
      Operation(opid(3), cid(2), Read, Output.UnknowRead, invocation = t(2), response = t(3)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 04") {
    val ops = List[Operation[Input, Output]](
      Operation(opid(1), cid(0), Write(State(1)), Output.Write, invocation = t(0), response = t(5)),
      Operation(opid(2), cid(1), Read, Output.Read(State(1)), invocation = t(1),  response = t(2)),
      Operation(opid(3), cid(2), Read, Output.Read((State())), invocation = t(3), response = t(4)),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Illegal)
  }
}

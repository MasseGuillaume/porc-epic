package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

class SetTest extends AnyFunSuite {

  import specification.SetModel._
  import Input._

  test("set 01") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = OperationId(1),
        clientId = ClientId(0), 
        input = Input.Write(state(100)),
        invocation = Time(0),
        output = Output.Write,
        response = Time(5)
      ),
      Operation(
        id = OperationId(2),
        clientId = ClientId(1), 
        input = Input.Write(state(0)), 
        invocation = Time(1), 
        output = Output.Write, 
        response = Time(4)
      ),
      Operation(
        id = OperationId(3),
        clientId = ClientId(2),
        input = Input.Read,
        invocation = Time(2),
        output = Output.Read(state(100)),
        response = Time(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 02") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = OperationId(1),
        clientId = ClientId(0), 
        input = Input.Write(state(100)),
        invocation = Time(0),
        output = Output.Write,
        response = Time(5)
      ),
      Operation(
        id = OperationId(2),
        clientId = ClientId(1), 
        input = Input.Write(state(110)),
        invocation = Time(1), 
        output = Output.Write, 
        response = Time(4)
      ),
      Operation(
        id = OperationId(3),
        clientId = ClientId(2),
        input = Input.Read,
        invocation = Time(2),
        output = Output.Read(state(100, 110)),
        response = Time(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 03") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = OperationId(1),
        clientId = ClientId(0), 
        input = Input.Write(state(100)),
        invocation = Time(0),
        output = Output.Write,
        response = Time(5)
      ),
      Operation(
        id = OperationId(2),
        clientId = ClientId(1), 
        input = Input.Write(state(110)),
        invocation = Time(1), 
        output = Output.Write, 
        response = Time(4)
      ),
      Operation(
        id = OperationId(3),
        clientId = ClientId(2),
        input = Input.Read,
        invocation = Time(2),
        output = Output.UnknowRead,
        response = Time(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 04") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = OperationId(1),
        clientId = ClientId(0), 
        input = Input.Write(state(1)),
        invocation = Time(0),
        output = Output.Write,
        response = Time(5)
      ),
      Operation(
        id = OperationId(2),
        clientId = ClientId(1), 
        input = Input.Read,
        invocation = Time(1), 
        output = Output.Read(state(1)),
        response = Time(2)
      ),
      Operation(
        id = OperationId(3),
        clientId = ClientId(2),
        input = Input.Read,
        invocation = Time(3),
        output = Output.Read((state())),
        response = Time(4)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Illegal)
  }
}

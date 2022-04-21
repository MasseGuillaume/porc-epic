package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class SetTest extends AnyFunSuite {

  import specification.SetModel._
  import Input._

  test("set 01") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = opid(1),
        clientId = cid(0), 
        input = Input.Write(state(100)),
        invocation = t(0),
        output = Output.Write,
        response = t(5)
      ),
      Operation(
        id = opid(2),
        clientId = cid(1), 
        input = Input.Write(state(0)), 
        invocation = t(1), 
        output = Output.Write, 
        response = t(4)
      ),
      Operation(
        id = opid(3),
        clientId = cid(2),
        input = Input.Read,
        invocation = t(2),
        output = Output.Read(state(100)),
        response = t(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 02") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = opid(1),
        clientId = cid(0), 
        input = Input.Write(state(100)),
        invocation = t(0),
        output = Output.Write,
        response = t(5)
      ),
      Operation(
        id = opid(2),
        clientId = cid(1), 
        input = Input.Write(state(110)),
        invocation = t(1), 
        output = Output.Write, 
        response = t(4)
      ),
      Operation(
        id = opid(3),
        clientId = cid(2),
        input = Input.Read,
        invocation = t(2),
        output = Output.Read(state(100, 110)),
        response = t(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 03") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = opid(1),
        clientId = cid(0), 
        input = Input.Write(state(100)),
        invocation = t(0),
        output = Output.Write,
        response = t(5)
      ),
      Operation(
        id = opid(2),
        clientId = cid(1), 
        input = Input.Write(state(110)),
        invocation = t(1), 
        output = Output.Write, 
        response = t(4)
      ),
      Operation(
        id = opid(3),
        clientId = cid(2),
        input = Input.Read,
        invocation = t(2),
        output = Output.UnknowRead,
        response = t(3)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("set 04") {
    val ops = List[Operation[Input, Output]](
      Operation(
        id = opid(1),
        clientId = cid(0), 
        input = Input.Write(state(1)),
        invocation = t(0),
        output = Output.Write,
        response = t(5)
      ),
      Operation(
        id = opid(2),
        clientId = cid(1), 
        input = Input.Read,
        invocation = t(1), 
        output = Output.Read(state(1)),
        response = t(2)
      ),
      Operation(
        id = opid(3),
        clientId = cid(2),
        input = Input.Read,
        invocation = t(3),
        output = Output.Read((state())),
        response = t(4)
      ),
    )
    val (result, info) = specification.checkOperations(ops)
    println(info.partialLinearizations.map(_.map(_.toList).toList).toList)
    assert(result == CheckResult.Illegal)
  }
}

package porcEpic

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class RegisterTest extends AnyFunSuite {

  opaque type State = Int

  enum Input:
    case Put(value: State) extends Input
    case Get extends Input

  import Input._

  val specification = new OperationSpecification[State, Input]{

    def initialState: State = 0

    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Put(value) => (true, value)
        case Get => (output == state, state)
      }
    }

    def describeOperation(input: Input, output: State): String = {
      input match {
        case Put(value) => s"put($value)"
        case Get => s"get() -> $output"
      }
    }

    def partitionOperations(
      operations: List[Operation[State, Input]]
    ): List[List[Operation[State, Input]]] =
      List(operations)
  }

  test("linearizable") {
    /*    0   1   2   3   4   5   6   7   8   9   10 |
      C0: |<---------------- Put(1)-------------->|  |
      C1:         |<------Get(1)----->|              |
      C2:             |<---Get(0)---->|              |
    */
    val ops = List(
      Operation(clientId = cid(0), input = Put(1), invocation = t(0), output = 0, response = t(10)),
      Operation(clientId = cid(1), input = Get,    invocation = t(2), output = 1, response = t(7)),
      Operation(clientId = cid(2), input = Get,    invocation = t(3), output = 0, response = t(7)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("not linearizable") {
    /*    0   1   2   3   4   5   6   7   8   9   10  |
      C0: |<--------------- Put(1) --------------->|  |
      C1:     |<--Get(1)->|                           |
      C2:                     |<-------Get(0)----->|  |
    */

    val ops = List(
      Operation(clientId = cid(0), input = Put(1), invocation = t(0), output = 0, response = t(10)),
      Operation(clientId = cid(1), input = Get,    invocation = t(1), output = 1, response =  t(4)),
      Operation(clientId = cid(2), input = Get,    invocation = t(5), output = 0, response = t(10)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Illgal)
  }
}

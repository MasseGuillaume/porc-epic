package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class RegisterTest extends AnyFunSuite {

  import specification.Register._
  import Input._

  test("linearizable") {
    /*    0   1   2   3   4   5   6   7   8   9   10 |
      C0: |<---------------- Put(1)-------------->|  |
      C1:         |<------Get(1)----->|              |
      C2:             |<---Get(0)---->|              |
    */
    val ops = List(
      Operation(clientId = cid(0), input = Put(state(1)), invocation = t(0), output = state(0), response = t(10)),
      Operation(clientId = cid(1), input = Get,           invocation = t(2), output = state(1), response = t(7)),
      Operation(clientId = cid(2), input = Get,           invocation = t(3), output = state(0), response = t(7)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("not-linearizable") {
    /*    0   1   2   3   4   5   6   7   8   9   10  |
      C0: |<--------------- Put(1) --------------->|  |
      C1:     |<--Get(1)->|                           |
      C2:                     |<-------Get(0)----->|  |
    */

    val ops = List(
      Operation(clientId = cid(0), input = Put(state(1)), invocation = t(0), output = state(0), response = t(10)),
      Operation(clientId = cid(1), input = Get,           invocation = t(1), output = state(1), response = t( 4)),
      Operation(clientId = cid(2), input = Get,           invocation = t(5), output = state(0), response = t(10)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Illegal)
  }
}

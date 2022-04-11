package porcEpic

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class PorcEpicTest extends AnyFunSuite {

  enum Input:
    case Put(value: Int) extends Input
    case Get extends Input

  import Input._

  given Eq[Int] with
    def equal(a: Int, b: Int): Boolean = a == b

  given Show[Int] with
    def show(a: Int): String = a.toString

  val specification = new Specification[Int, Input]{

    def initialState: Int = 0

    def apply(state: Int, input: Input, output: Int): (Boolean, Int) = {
      input match {
        case Put(value) => (true, value)
        case Get => (output == state, state)
      }
    }

    def describeOperation(input: Input, output: Int): String = {
      input match {
        case Put(value) => s"put($value)"
        case Get => s"get() -> $output"
      }
    }
  }

  
  test("specification 1") {
    /*    0        25  30      50     60  75        100 |
      C0: |<--------------- Put(100) --------------->|  |
      C1:          |<---------Get--------->|            |
      C2:               |<----Get----->|                |
    */
    val ops = List(
      Operation(clientId = cid(0), input = Put(100), invocation =  t(0), output =   0, response = t(100)),
      Operation(clientId = cid(1), input = Get,      invocation = t(25), output = 100, response = t(75)),
      Operation(clientId = cid(2), input = Get,      invocation = t(30), output =   0, response = t(60)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("specification 2") {
    val ops = List(
      Operation(clientId = cid(0), input = Get, invocation = t(0), output = 0, response = t(1)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }

  test("specification 3") {
    val ops = List(
      Operation(clientId = cid(0), input = Put(1), invocation = t(0), output = 0, response = t(3)),
      Operation(clientId = cid(0), input = Get,    invocation = t(0), output = 1, response = t(3)),
    )
    val (result, _) = specification.checkOperations(ops)
    assert(result == CheckResult.Ok)
  }
}

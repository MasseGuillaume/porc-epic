package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

import porcEpic.{fromLong => t}

class VisualizationTest extends AnyFunSuite {

  import specification.Register._
  import Input._

  
  def describeOperation(operation: Operation[Input, Output]): String =
    operation.input match {
      case Put(value) => s"put($value)"
      case Get => s"get() -> ${operation.output}"
    }

  def describeState(state: State): String =
    state.toString

  test("linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = opid(0), clientId = cid(0), input = Put(state(1)), invocation = t(0), output = output(0), response = t(10)),
      Operation(id = opid(1), clientId = cid(1), input = Get,           invocation = t(2), output = output(1), response = t(7)),
      Operation(id = opid(2), clientId = cid(2), input = Get,           invocation = t(3), output = output(0), response = t(7)),
    )
    val (_, info) = model.checkOperations(ops)
    val data = 
      model.visualize(
        info.get,
        describeOperation,
        describeState
      )
    
    data.head.History.foreach(println)
  }

  test("not-linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = opid(0), clientId = cid(0), input = Put(state(1)), invocation = t(0), output = output(0), response = t(10)),
      Operation(id = opid(1), clientId = cid(1), input = Get,           invocation = t(1), output = output(1), response = t( 4)),
      Operation(id = opid(2), clientId = cid(2), input = Get,           invocation = t(5), output = output(0), response = t(10)),
    )
    val (result, info) = model.checkOperations(ops)
    assert(info.get.partialLinearizations == List(List(List(0, 1))))
    assert(result == CheckResult.Illegal)
  }
}

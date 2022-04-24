package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

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

  test("not-linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = OperationId(0), clientId = ClientId(0), input = Put(state(1)), invocation = Time(0), output = output(0), response = Time(10)),
      Operation(id = OperationId(1), clientId = ClientId(1), input = Get,           invocation = Time(1), output = output(1), response = Time( 4)),
      Operation(id = OperationId(2), clientId = ClientId(2), input = Get,           invocation = Time(5), output = output(0), response = Time(10)),
    )
    val (result, info) = model.checkOperations(ops)
    assert(info.get.partialLinearizations == List(List(List(0, 1))))
    assert(result == CheckResult.Illegal)

    val data = 
      model.visualize(
        info.get,
        describeOperation,
        describeState
      )

    assert(data.length == 1)

    assert(
      data.head.LargestIndex ==
        Map(
          0 -> 0,
          1 -> 0
        )
    )

    assert(
      data.head.PartialLinearizations ==
        List(
          List(
            LinearizationStep(OperationId(0), "1"),
            LinearizationStep(OperationId(1), "1")
          )
        )
    )

    data.save("data-test.js")
  }

  test("linearizable") {
    val ops = List[Operation[Input, Output]](
      Operation(id = OperationId(0), clientId = ClientId(0), input = Put(state(1)), invocation = Time(0), output = output(0), response = Time(10)),
      Operation(id = OperationId(1), clientId = ClientId(1), input = Get,           invocation = Time(2), output = output(1), response = Time(8)),
      Operation(id = OperationId(2), clientId = ClientId(2), input = Get,           invocation = Time(3), output = output(0), response = Time(7)),
    )
    val (_, info) = model.checkOperations(ops)
    val data = 
      model.visualize(
        info.get,
        describeOperation,
        describeState
      )
    
    assert(data.length == 1)
    
    assert(
      data.head.LargestIndex == 
        Map(
          0 -> 0,
          1 -> 0,
          2 -> 0
        )
    )

    assert(
      data.head.PartialLinearizations ==
        List(
          List(
            LinearizationStep(OperationId(2), "0"),
            LinearizationStep(OperationId(0), "1"),
            LinearizationStep(OperationId(1), "1")
          )
        )
    )

    data.save("data2-test.js")
  }
}

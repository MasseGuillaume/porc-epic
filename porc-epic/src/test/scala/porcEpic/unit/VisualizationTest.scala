package porcEpic
package unit

class VisualizationTest extends munit.FunSuite {

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
    val history = List[Operation[Input, Output]](
      Operation(OperationId(1), ClientId(0), Put(State(1)), Output(0), invocation = Time(0), response = Time(10)),
      Operation(OperationId(2), ClientId(1), Get,           Output(1), invocation = Time(2), response = Time(7)),
      Operation(OperationId(3), ClientId(2), Get,           Output(0), invocation = Time(3), response = Time(7)),
    )
    val (_, Some(info)) = model.checkOperations(history)

    val data = 
      model.visualize(
        info,
        describeOperation,
        describeState
      )

    assertEquals(data.length, 1)

    // assertEquals(
    //   data.head.LargestIndex,
    //     Map(
    //       0 -> 0,
    //       1 -> 0
    //     )
    // )

    // assertEquals(
    //   data.head.PartialLinearizations,
    //     List(
    //       List(
    //         LinearizationStep(OperationId(0), "1"),
    //         LinearizationStep(OperationId(1), "1")
    //       )
    //     )
    // )

    println(data.save())
  }

  test("linearizable") {
    val history = List[Operation[Input, Output]](
      Operation(OperationId(1), ClientId(0), Put(State(1)), Output(0), invocation = Time(0), response = Time(10)),
      Operation(OperationId(2), ClientId(1), Get,           Output(1), invocation = Time(1), response = Time( 4)),
      Operation(OperationId(3), ClientId(2), Get,           Output(0), invocation = Time(5), response = Time(10)),
    )
    val (_, info) = model.checkOperations(history)
    val data = 
      model.visualize(
        info.get,
        describeOperation,
        describeState
      )
    
    // assertEquals(
    //   data.head.LargestIndex, 
    //     Map(
    //       0 -> 0,
    //       1 -> 0,
    //       2 -> 0
    //     )
    // )

    // assertEquals(
    //   data.head.PartialLinearizations,
    //     List(
    //       List(
    //         LinearizationStep(OperationId(2), "0"),
    //         LinearizationStep(OperationId(0), "1"),
    //         LinearizationStep(OperationId(1), "1")
    //       )
    //     )
    // )

    println(data.save())
  }
}

package porcEpic
package specification

object Register {
  opaque type State = Int
  def state(value: Int): State = value 

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
}
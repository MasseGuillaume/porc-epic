package porcEpic
package specification

object Register {
  opaque type State = Int
  def state(value: Int): State = value 

  opaque type Output = State
  def output(value: Int): Output = value 

  enum Input:
    case Put(value: State) extends Input
    case Get extends Input

  import Input._

  val specification = new OperationSpecification[State, Input, Output]{

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
  }
}
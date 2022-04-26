package porcEpic
package specification

object Register {
  opaque type State = Int
  object State {
    def apply(value: Int): State = value 
  }
  opaque type Output = State
  object Output {
    def apply(value: Int): Output = value 
  }

  enum Input:
    case Put(value: State)
    case Get

  import Input._

  val model = new OperationSpecification[State, Input, Output]{
    def initialState: State = 0
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Put(value) => (true, value)
        case Get => (output == state, state)
      }
    }
  }
}
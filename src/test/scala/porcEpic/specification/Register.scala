package porcEpic
package specification

object Register {
  opaque type State = Int
  object State {
    def apply(value: Int): State = value 
  }
  enum Input:
    case Put(value: State)
    case Get
  opaque type Output = State
  object Output {
    def apply(value: Int): Output = value 
  }
  val model = new OperationSpecification[State, Input, Output]{
    def initialState: State = 0
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      import Input._
      input match {
        case Put(value) => (true, value)
        case Get        => (output == state, state)
      }
    }
  }
}
package porcEpic
package specification

object Etcd {
  opaque type State = Int
  def state(value: Int): State = value

  enum Input:
    case Read extends Input
    case Write(value: State) extends Input
    case Cas(expected: State, value: State) extends Input

  enum Output:
    case Timeout extends Output
    case Cas(ok: Boolean) extends Output
    case Read(value: Option[State]) extends Output
    case Write(value: State) extends Output

  val specification = new EntriesSpecification[Option[State], Input, Output]{
    def initialState: Option[State] = None

    def apply(state: Option[State], input: Input, output: Output): (Boolean, Option[State]) = {
      (input, output) match {
        case (Input.Read                , Output.Read(outputState)) => (state == outputState, state)
        case (Input.Read                , Output.Timeout)           => (true, state)

        case (Input.Write(value)        , _)                        => (true, Some(value))

        case (Input.Cas(expected, value), _) => {
          val ok = 
            output match {
              case Output.Cas(ok) => if (ok) Some(expected) == state else Some(expected) != state
              case Output.Timeout => true
              case _              => throw new Exception(s"invalid cas output: $output")
            }
            
          val result =
            if (Some(expected) == state) Some(value)
            else state

          (ok, result)
        }
        
        case _ => throw new Exception(s"invalid history: $state, $input, $output")
      }
    }

    def describeOperation(input: Input, output: Output): String = {
      (input, output) match {
        case (Input.Read                , Output.Read(state))  => s"""read() -> ${state.getOrElse("nil")}"""
        case (Input.Write(value)        , Output.Write(state)) => s"write(${state})"
        case (Input.Cas(expected, value), Output.Cas(ok))      => s"cas($expected, $value) -> $ok"
        case (_                         , Output.Timeout)      => "timeout"
        case _                                                 => throw new Exception("invalid operation")
      }
    }
  }
}


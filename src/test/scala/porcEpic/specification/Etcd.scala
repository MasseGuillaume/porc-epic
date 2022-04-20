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
    case CasFail extends Output
    case CasOk extends Output
    case Read(value: Option[State]) extends Output
    case Write(value: State) extends Output

  val specification = new EntriesSpecification[Option[State], Input, Output]{
    def initialState: Option[State] = None

    def apply(state: Option[State], input: Input, output: Output): (Boolean, Option[State]) = {
      (input, output) match {
        case (Input.Read        , Output.Read(outputState)) => (state == outputState, state)
        case (Input.Write(value), _)                        => (true, Some(value))
        case (_                 , Output.Timeout)           => (true, state)
        case (Input.Cas(expected, value), Output.CasFail)   => (Some(expected) != state, state)
        case (Input.Cas(expected, value), Output.CasOk) => {
          val ok = Some(expected) == state
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
        case (Input.Read, _)                 => ???
        case (Input.Write(value), _)         => ???
        case (Input.Cas(expected, value), _) => ???
      }
    }
  }
}

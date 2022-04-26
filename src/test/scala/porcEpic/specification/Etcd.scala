package porcEpic
package specification

object Etcd {
  opaque type State = Int
  object State {
    def apply(value: Int): State = value
  }
  enum Input:
    case Read
    case Write(value: State)
    case Cas(expected: State, value: State)

  enum Output:
    case Unknown
    case Cas(ok: Boolean)
    case Read(value: Option[State])
    case Write(value: State)

  val specification = new EntriesSpecification[Option[State], Input, Output]{
    def initialState: Option[State] = None

    def apply(state: Option[State], input: Input, output: Output): (Boolean, Option[State]) = {
      (input, output) match {
        case (Input.Read                , Output.Read(outputState)) => (state == outputState, state)
        case (Input.Read                , Output.Unknown)           => (true, state)
        case (Input.Write(value)        , _)                        => (true, Some(value))

        case (Input.Cas(expected, value), _) => {
          val ok = 
            output match {
              case Output.Cas(ok) => if (ok) Some(expected) == state 
                                     else Some(expected) != state
              case Output.Unknown => true
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
  }
}

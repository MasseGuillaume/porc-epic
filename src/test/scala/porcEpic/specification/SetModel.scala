package porcEpic
package specification

object SetModel {
  opaque type State = Set[Int]

  def state(value: Int*): State = Set(value*)

  enum Input:
    case Read
    case Write(values: State)

  enum Output:
    case Write
    case UnknowRead
    case Read(values: State)


  import Input._

  val specification = new OperationSpecification[State, Input, Output]{

    def initialState: State = Set.empty

    def apply(state: State, input: Input, output: Output): (Boolean, State) = {

      def invalid = throw new Exception("invalid history: $state, $input, $output")

      (input, output) match {
        case (Input.Write(values), Output.Write)        => (true           , state ++ values)
        case (Input.Read         , Output.UnknowRead)   => (true           , state          )
        case (Input.Read         , Output.Read(values)) => (state == values, values         )
        case _                                          => invalid
      }
    }
  }
}

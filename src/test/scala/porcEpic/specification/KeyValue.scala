package porcEpic
package specification

object KeyValue {
  opaque type State = String
  def state(value: String): State = value

  extension (state: State) {
    def +(other: State): State = state + other
  }

  sealed trait Input {
    val key: String
  }
  case class Put(key: String, value: State) extends Input
  case class Get(key: String) extends Input
  case class Append(key: String, value: State) extends Input

  val specification = new EntriesSpecification[State, Input]{
    def partitionEntries(entries: List[Entry[State, Input]]): List[List[Entry[State, Input]]] = {
      Entry.toOperations(entries)
        .groupBy(_.input.key)
        .values
        .map(Entry.fromOperations)
        .toList
    }
    def initialState: State = state("")
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Get(key)           => (output == state, state)
        case Put(key, value)    => (true, value)
        case Append(key, value) => (true, state + value)
      }
    }
    def describeOperation(input: Input, output: State): String = {
      input match {
        case Get(key)           => s"get($key) -> $output"
        case Put(key, value)    => s"put($key, $value)"
        case Append(key, value) => s"append($key, $value)"
      }
    }
  }
}
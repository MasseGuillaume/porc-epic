package porcEpic
package specification

object KeyValue {
  opaque type State = String
  object State {
    def apply(value: String): State = value
  }
  extension (state: State) {
    def +(other: State): State = state + other
  }
  sealed trait Input {
    val key: String
  }
  object Input {
    case class Put(key: String, value: State) extends Input
    case class Get(key: String) extends Input
    case class Append(key: String, value: State) extends Input
  }
  opaque type Output = State
  object Output {
    def apply(value: String): Output = value
  }

  val specification = new EntriesSpecification[State, Input, Output] {
    override def partitionEntries(entries: List[Entry[Input, Output]]): List[List[Entry[Input, Output]]] = {
      Entry
        .toOperations(entries)
        .groupBy(_.input.key)
        .values
        .map(Entry.fromOperations)
        .toList
    }
    def initialState: State = State("")
    def apply(state: State, input: Input, output: State): (Boolean, State) = {
      input match {
        case Input.Get(key)           => (output == state, state)
        case Input.Put(key, value)    => (true, value)
        case Input.Append(key, value) => (true, state + value)
      }
    }
  }
}

object KeyValueTest {
  def names = List(
    "c01-bad",
    "c01-ok",
    "c10-bad",
    "c10-ok",
    "c50-ok",
    "c50-bad", // this test takes a bit more time
  )
}
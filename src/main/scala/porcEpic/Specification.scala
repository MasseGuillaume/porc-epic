package porcEpic

opaque type Time = Long
def fromLong(value: Long): Time = value
def toLong(time: Time): Long = time

opaque type ClientId = Int
def cid(value: Int): ClientId = value

object Operation {
  def apply[State, Input, Output](call: Entry.Call[Input, Output], `return`: Entry.Return[Input, Output]): Operation[Input, Output] = 
    Operation(
      clientId = call.clientId,
      input = call.value,
      invocation = call.time,
      output = `return`.value,
      response = `return`.time,
    )
}

case class Operation[Input, Output](
  clientId: ClientId,
  input: Input,
  invocation: Time,
  output: Output,
  response: Time
)

sealed trait Entry[Input, Output] {
  val time: Time
  val id: Int
  val clientId: ClientId

  def withId(id0: Int): Entry[Input, Output] = {
    this match {
      case c: Entry.Call[_, _]   => c.copy(id = id0)
      case r: Entry.Return[_, _] => r.copy(id = id0)
    }
  }
}

object Entry {
  case class   Call[Input, Output](value: Input, time: Time, id: Int, clientId: ClientId) extends Entry[Input, Output]
  case class Return[Input, Output](value: Output, time: Time, id: Int, clientId: ClientId) extends Entry[Input, Output]

  def fromOperations[Input, Output](history: List[Operation[Input, Output]]): List[Entry[Input, Output]] = {
    history.zipWithIndex.flatMap ( (operation, index) =>
      List[Entry[Input, Output]](
          Call(operation.input,  operation.invocation, index, operation.clientId),
        Return(operation.output, operation.response,   index, operation.clientId)
      )
    ).sorted
  }

  def toOperations[Input, Output](history: List[Entry[Input, Output]]): List[Operation[Input, Output]] = {
    history.groupBy(_.id).map {
      case (_, List(c: Entry.Call[_, _],   r: Entry.Return[_, _])) => Operation(c, r)
      case (_, List(r: Entry.Return[_, _], c: Entry.Call[_, _])) => Operation(c, r)
      case (id, entries) => 
        throw new Exception(
          s"""|history is not complete for id $id:
              |${entries.mkString("\n")}""".stripMargin
        )
    }.toList
  }
}

trait Specification[State, Input, Output] {
  def initialState: State
  def equal(state1: State, state2: State): Boolean = state1 == state2
  def apply(state: State, input: Input, output: Output): (Boolean, State)
}

trait OperationSpecification[State, Input, Output] extends Specification[State, Input, Output] {
  def partitionOperations(operations: List[Operation[Input, Output]]): List[List[Operation[Input, Output]]] = List(operations)
}

trait EntriesSpecification[State, Input, Output] extends Specification[State, Input, Output] {
  def partitionEntries(entries: List[Entry[Input, Output]]): List[List[Entry[Input, Output]]] = List(entries)
}

enum CheckResult:
  case TimedOut
  case Ok
  case Illegal

case class LinearizationInfo[Input, Output](
  history: List[List[Entry[Input, Output]]],
  partialLinearizations: List[List[List[Int]]]
)

object LinearizationInfo {
  def empty[Input, Output]: LinearizationInfo[Input, Output] = 
    LinearizationInfo(Nil, Nil)
}

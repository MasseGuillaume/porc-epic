package porcEpic

opaque type Time = Long
object Time {
  def apply(value: Long): Time = value
  def toLong(time: Time): Long = time
}

opaque type ClientId = Int
object ClientId {
  def apply(value: Int): ClientId = value
  def toInt(cliendId: ClientId): Int = cliendId
}

object Operation {
  def apply[State, Input, Output](call: Entry.Call[Input, Output], `return`: Entry.Return[Input, Output]): Operation[Input, Output] = 
    Operation(
      id = call.id,
      clientId = call.clientId,
      input = call.value,
      invocation = call.time,
      output = `return`.value,
      response = `return`.time,
    )
}

opaque type OperationId = Int
object OperationId {
  def apply(value: Int): OperationId = value
  def toInt(id: OperationId): Int = id
}

case class Operation[Input, Output](
  id: OperationId,
  clientId: ClientId,
  input: Input,
  output: Output,
  invocation: Time,
  response: Time
)

sealed trait Entry[Input, Output] {
  val time: Time
  val id: OperationId
  val clientId: ClientId

  def withId(id0: OperationId): Entry[Input, Output] = {
    this match {
      case c: Entry.Call[_, _]   => c.copy(id = id0)
      case r: Entry.Return[_, _] => r.copy(id = id0)
    }
  }
}

object Entry {
  case class   Call[Input, Output](value: Input , time: Time, id: OperationId, clientId: ClientId) extends Entry[Input, Output]
  case class Return[Input, Output](value: Output, time: Time, id: OperationId, clientId: ClientId) extends Entry[Input, Output]

  def fromOperations[Input, Output](history: List[Operation[Input, Output]]): List[Entry[Input, Output]] = {
    history.zipWithIndex.flatMap ( (operation, index) =>
      List[Entry[Input, Output]](
          Call(operation.input,  operation.invocation, OperationId(index), operation.clientId),
        Return(operation.output, operation.response,   OperationId(index), operation.clientId)
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
  partialLinearizations: List[List[List[OperationId]]]
)

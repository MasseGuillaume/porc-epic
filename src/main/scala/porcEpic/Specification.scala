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
  def apply[S, I, O](call: Entry.Call[I, O], `return`: Entry.Return[I, O]): Operation[I, O] = 
    Operation(
      id = call.id,
      clientId = call.clientId,
      input = call.input,
      invocation = call.time,
      output = `return`.output,
      response = `return`.time,
    )
}

opaque type OperationId = Int
object OperationId {
  def apply(value: Int): OperationId = value
  def toInt(id: OperationId): Int = id
}

case class Operation[I, O](
  id: OperationId,
  clientId: ClientId,
  input: I,
  output: O,
  invocation: Time,
  response: Time
)

sealed trait Entry[I, O] {
  val time: Time
  val id: OperationId
  val clientId: ClientId

  def withId(id0: OperationId): Entry[I, O] = {
    this match {
      case c: Entry.Call[_, _]   => c.copy(id = id0)
      case r: Entry.Return[_, _] => r.copy(id = id0)
    }
  }
}

object Entry {
  case class   Call[I, O](input: I, time: Time, id: OperationId, clientId: ClientId) extends Entry[I, O]
  case class Return[I, O](output: O, time: Time, id: OperationId, clientId: ClientId) extends Entry[I, O]

  def fromOperations[I, O](history: List[Operation[I, O]]): List[Entry[I, O]] = {
    history.zipWithIndex.flatMap ( (operation, index) =>
      List[Entry[I, O]](
          Call(operation.input,  operation.invocation, OperationId(index), operation.clientId),
        Return(operation.output, operation.response,   OperationId(index), operation.clientId)
      )
    ).sorted
  }

  def toOperations[I, O](history: List[Entry[I, O]]): List[Operation[I, O]] = {
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

trait Specification[S, I, O] {
  def initialState: S
  def equal(state1: S, state2: S): Boolean = state1 == state2
  def apply(state: S, input: I, output: O): (Boolean, S)
}

trait OperationSpecification[S, I, O] extends Specification[S, I, O] {
  def partitionOperations(operations: List[Operation[I, O]]): List[List[Operation[I, O]]] = List(operations)
}

trait EntriesSpecification[S, I, O] extends Specification[S, I, O] {
  def partitionEntries(entries: List[Entry[I, O]]): List[List[Entry[I, O]]] = List(entries)
}

enum CheckResult:
  case TimedOut
  case Ok
  case Illegal

case class LinearizationInfo[I, O](
  history: List[List[Entry[I, O]]],
  partialLinearizations: List[List[List[OperationId]]]
)

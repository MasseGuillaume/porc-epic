package porcEpic

opaque type Time = Long
def fromLong(value: Long): Time = value
def toLong(time: Time): Long = time

opaque type ClientId = Int
def cid(value: Int): ClientId = value

object Operation {
  def apply[S, T](call: Entry.Call[S, T], `return`: Entry.Return[S, T]): Operation[S, T] = 
    Operation(
      clientId = call.clientId,
      input = call.value,
      invocation = call.time,
      output = `return`.value,
      response = `return`.time,
    )
}

case class Operation[S, T](
  clientId: ClientId,
  input: T,
  invocation: Time,
  output: S,
  response: Time
)

sealed trait Entry[S, T] {
  val time: Time
  val id: Int
  val clientId: ClientId

  def withId(id0: Int): Entry[S, T] = {
    this match {
      case c: Entry.Call[_, _]   => c.copy(id = id0)
      case r: Entry.Return[_, _] => r.copy(id = id0)
    }
  }
}

object Entry {
  case class   Call[S, T](value: T, time: Time, id: Int, clientId: ClientId) extends Entry[S, T]
  case class Return[S, T](value: S, time: Time, id: Int, clientId: ClientId) extends Entry[S, T]

  def fromOperations[S, T](history: List[Operation[S, T]]): List[Entry[S, T]] = {
    history.zipWithIndex.flatMap ( (operation, index) =>
      List[Entry[S, T]](
          Call(operation.input,  operation.invocation, index, operation.clientId),
        Return(operation.output, operation.response,   index, operation.clientId)
      )
    ).sorted
  }

  def toOperations[S, T](history: List[Entry[S, T]]): List[Operation[S, T]] = {
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

trait Specification[State, Input] {
  def initialState: State
  def equal(state1: State, state2: State): Boolean = state1 == state2
  def apply(state: State, input: Input, output: State): (Boolean, State)
  def describeOperation(input: Input, output: State): String
}

trait OperationSpecification[S, T] extends Specification[S, T] {
  def partitionOperations(operations: List[Operation[S, T]]): List[List[Operation[S, T]]]
}

trait EntriesSpecification[S, T] extends Specification[S, T] {
  def partitionEntries(entries: List[Entry[S, T]]): List[List[Entry[S, T]]]
}

enum CheckResult:
  case TimedOut
  case Ok
  case Illegal

case class LinearizationInfo[S, T](
  history: List[List[Entry[S, T]]],
  partialLinearizations: List[List[List[Int]]]
)

object LinearizationInfo {
  def empty[S, T]: LinearizationInfo[S, T] = 
    LinearizationInfo(Nil, Nil)
}
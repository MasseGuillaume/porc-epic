package porcEpic
package jepsen


import scala.io.Source

import org.scalatest.funsuite.AnyFunSuite

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
}

class KeyValueTest extends AnyFunSuite {

  import KeyValue._

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

  def parse(filename: String): List[Entry[State, Input]] = {
    import KeyValueParser._
    import porcEpic.{fromLong => t}

    // val content = Files.readString(Paths.get(filename))
    val source = Source.fromFile(s"porcupine/test_data/kv/${filename}.txt")
    val entries = source.getLines.map(KeyValueParser.apply)

    val processToId = collection.mutable.Map.empty[Int, Int]
    var i = 0
    val events: List[Entry[State, KeyValue.Input]] = 
      entries.zipWithIndex.map {
        case (KeyValueEntry(process, EntryType.Invoke, functionType, key, maybeValue), time) =>
          val id = i
          i += 1
          processToId(process) = id
          val input: KeyValue.Input =
            (functionType, maybeValue) match {
              case (FunctionType.Get,    None)        => KeyValue.Get(key)
              case (FunctionType.Put,    Some(value)) => KeyValue.Put(key, state(value))
              case (FunctionType.Append, Some(value)) => KeyValue.Append(key, state(value))
              case _ => throw new Exception(s"bogus parsing: $filename $functionType $maybeValue")
            }
          Entry.Call[State, KeyValue.Input](
            value = input,
            time = t(time),
            id = id,
            clientId = cid(process)
          )

        case (KeyValueEntry(process, EntryType.Ok, _, key, Some(output)), time) =>
          val matchId = processToId(process)
          processToId -= process
          Entry.Return[State, KeyValue.Input](
            value = state(output),
            time = t(time),
            id = matchId,
            clientId = cid(process)
          )

        case other =>
          throw new Exception(s"bogus parsing: $filename, $other")
      }.toList

    source.close()

    events
  }

  List(
    "c01-bad",
    "c01-ok",
    "c10-bad",
    "c10-ok",
    "c50-bad",
    "c50-ok",
  ).foreach(name =>

    test(name) {
      val entries = parse(name)
      val (obtained, _) = specification.checkEntries(entries)

      val expected =
        if (name.endsWith("-bad")) CheckResult.Illegal
        else if (name.endsWith("-ok")) CheckResult.Ok
        else throw new Exception("invalid test name: " + name)

      assert(obtained == expected)
    }
  )
}

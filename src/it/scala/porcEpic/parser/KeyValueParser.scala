package porcEpic
package parser

import specification.KeyValue

import cats.parse.Parser
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser.{string, char, charsWhile0}
import cats.parse.Rfc5234.{digit, sp}

import cats.implicits.toShow

import scala.io.Source

object KeyValueParser {
  enum EntryType:
    case Invoke
    case Ok

  enum FunctionType:
    case Get
    case Put
    case Append

  case class KeyValueEntry(
    process: Int, 
    `type`: EntryType,
    f: FunctionType,
    key: String,
    value: Option[String]
  )

  def parseFile(filename: String): List[Entry[KeyValue.Input, KeyValue.Output]] = {
    val source = Source.fromFile(s"porcupine/test_data/kv/${filename}.txt")
    val entries = source.getLines.map(KeyValueParser.apply)

    val processToId = collection.mutable.Map.empty[Int, OperationId]
    var i = 0
    val events = 
      entries.zipWithIndex.map {
        case (KeyValueEntry(process, EntryType.Invoke, functionType, key, maybeValue), time) =>
          val id = i
          i += 1
          processToId(process) = OperationId(id)

          val input: KeyValue.Input =
            (functionType, maybeValue) match {
              case (FunctionType.Get,    None)        => KeyValue.Input.Get(key)
              case (FunctionType.Put,    Some(value)) => KeyValue.Input.Put(key, KeyValue.State(value))
              case (FunctionType.Append, Some(value)) => KeyValue.Input.Append(key, KeyValue.State(value))
              case _ => throw new Exception(s"bogus parsing: $filename $functionType $maybeValue")
            }

          Entry.Call[KeyValue.Input, KeyValue.Output](
            input,
            Time(time),
            OperationId(id),
            ClientId(process)
          )

        case (KeyValueEntry(process, EntryType.Ok, _, key, Some(output)), time) =>
          val matchId = processToId(process)
          processToId -= process
          Entry.Return[KeyValue.Input, KeyValue.Output](
            KeyValue.Output(output),
            Time(time),
            matchId,
            ClientId(process)
          )

        case other =>
          throw new Exception(s"bogus parsing: $filename, $other")
      }.toList

    source.close()

    events
  }

  def apply(input: String): KeyValueEntry = 
    root.parseAll(input).fold(
      error => throw new Exception(".\n" + error.toString),
      x => x
    )

  def digitInt : Parser[Int] = nonNegativeIntString.map(_.toInt)

  def sep: Parser[Any] = string(",") ~ sp.rep

  def root: Parser[KeyValueEntry] =
    (
       (string(":process") *> sp.rep *> digitInt     <* sep) ~
       (string(":type")    *> sp.rep *> entryType    <* sep) ~
       (string(":f")       *> sp.rep *> functionType <* sep) ~
       (string(":key")     *> sp.rep *> quotedString <* sep) ~
       (string(":value")   *> sp.rep *> optValue)
    ).between(string("{"), string("}")).map {
      case ((((process, tpe), f), key), value) =>
        KeyValueEntry(process, tpe, f, key, value)
    }

  def entryType: Parser[EntryType] =
    string(":invoke").map( _ => EntryType.Invoke ) |
    string(":ok").map( _ => EntryType.Ok )

  def functionType: Parser[FunctionType] =
    string(":get").map( _ => FunctionType.Get ) |
    string(":put").map( _ => FunctionType.Put ) |   
    string(":append").map( _ => FunctionType.Append )

  def dq: Parser[Unit] = char('"')

  def quotedString: Parser[String] = (dq *> charsWhile0(_ != '"') <* dq)

  def optValue: Parser[Option[String]] =
    string("nil").map(_ => None) | 
    quotedString.map(str => Some(str.trim))
}

package porcEpic
package parser

import specification.KeyValue

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical

import scala.io.Source

object KeyValueParser extends RegexParsers {
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
    parseAll(root, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        val offset = failure.next.offset
        val carret = (1 to offset).map(_ => " ").mkString("") + "^"
        throw new Exception(
          s"""|${failure.msg}
              |$input
              |$carret""".stripMargin
        )
    }

  def root: Parser[KeyValueEntry] =
    "{" ~> 
       (":process" ~> int <~ ",") ~
       (":type" ~> entryType <~ ",") ~
       (":f" ~> functionType <~ ",") ~
       (":key " ~> string <~ ",") ~
       (":value " ~> optString) <~
    "}" ^^ {
      case process ~ tpe ~ f ~ key ~ value =>
        KeyValueEntry(process, tpe, f, key, value)
    }

  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def entryType: Parser[EntryType] =
    ":invoke" ^^ { _ => EntryType.Invoke } |
    ":ok"     ^^ { _ => EntryType.Ok }

  def functionType: Parser[FunctionType] =
    ":get"    ^^ { _ => FunctionType.Get } |
    ":put"    ^^ { _ => FunctionType.Put } |   
    ":append" ^^ { _ => FunctionType.Append }

  val dq = '"'
  def string: Parser[String] =
    dq ~> rep(elem("string", _ != dq)) <~ dq ^^ { _.mkString("") }

  def optString: Parser[Option[String]] =
    "nil" ^^ { _ => None} | 
    string.map(Some(_))
}
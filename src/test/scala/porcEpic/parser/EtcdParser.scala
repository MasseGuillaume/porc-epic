package porcEpic
package parser

import specification.Etcd

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical

import scala.io.Source

object EtcdParser extends RegexParsers {
  enum EntryType:
    case Invoke extends EntryType
    case Ok extends EntryType
    case Fail extends EntryType
    case Info extends EntryType

  enum FunctionType:
    case Read extends FunctionType
    case Write extends FunctionType
    case Cas extends FunctionType

  enum Val:
    case Nil extends Val
    case Value(value: Int) extends Val
    case CasValue(expected: Int, value: Int) extends Val
    case Unknown extends Val

  case class EtcdEntry(
    process: Int, 
    entry: EntryType,
    function: FunctionType,
    value: Val
  )

  def parseFile(index: String): List[Entry[Etcd.Input, Etcd.Output]] = {
    import porcEpic.{fromLong => t}

    import FunctionType._
    import EntryType._
    import Etcd.state
    
    val filename = leftPad(index)(3, '0')
    val source = Source.fromFile(s"porcupine/test_data/jepsen/etcd_${filename}.log")
    val entries = source.getLines.map(parse)

    val processToId = collection.mutable.Map.empty[Int, OperationId]
    var i = 0
    val events = 
      entries.zipWithIndex.flatMap {
        case (EtcdEntry(process, EntryType.Invoke, function, value), time) =>
          val id = i
          i += 1
          processToId(process) = opid(id)

          val input =
            (function, value) match {
              case (Read,  Val.Nil)                       => Etcd.Input.Read
              case (Write, Val.Value(value))              => Etcd.Input.Write(state(value))
              case (Cas,   Val.CasValue(expected, value)) => Etcd.Input.Cas(state(expected), state(value))
              case _ => throw new Exception(s"bogus parsing: $filename $functionType $value")
            }

          Some(
            Entry.Call[Etcd.Input, Etcd.Output](
              value = input,
              time = t(time),
              id = opid(id),
              clientId = cid(process)
            )
          )


        case (EtcdEntry(process, entry, function, value), time) =>
          val output: Option[Etcd.Output] =
            (entry, function, value) match {
              case (Ok  , Read  , Val.Value(value))   => Some(Etcd.Output.Read(Some(state(value))))
              case (Ok  , Read  , Val.Nil)            => Some(Etcd.Output.Read(None))
              case (Ok  , Write , Val.Value(value))   => Some(Etcd.Output.Write(state(value)))
              case (Fail, _     , Val.CasValue(_, _)) => Some(Etcd.Output.Cas(ok = false))
              case (Ok  , _     , Val.CasValue(_, _)) => Some(Etcd.Output.Cas(ok = true))
              case (_   , Read  , Val.Unknown)        => Some(Etcd.Output.Unknown)
              case (_   , _     , Val.Unknown)        => None
              case _                                  => throw new Exception(s"unexpected return entry: $entry, $function, $value")
            }

          output.map{ out =>

            val matchId = processToId(process)
            processToId -= process

            Entry.Return[Etcd.Input, Etcd.Output](
              value = out,
              time = t(time),
              id = matchId,
              clientId = cid(process)
            )
          }

      }.toList

    // events with call but without returns
    // we dropped cas and write timeout since we don't know if the server got the packet or not
    val lastTime = events.length
    val unfinishedEvents =
      processToId.toList.zipWithIndex.map { case ((process, matchId), time) =>
        Entry.Return[Etcd.Input, Etcd.Output](
          value = Etcd.Output.Unknown,
          time = t(lastTime + time),
          id = matchId,
          clientId = cid(process)
        )        
      }

    source.close()

    events ::: unfinishedEvents
  }

  def parse(input: String): EtcdEntry = 
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

  def root: Parser[EtcdEntry] =
    ("INFO  jepsen.util -" ~> int) ~ entryType ~ functionType ~ value ^^ {
      case process ~ eTpe ~ fTpe ~ value =>
        EtcdEntry(process, eTpe, fTpe, value)
    }

  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def value: Parser[Val] = 
    "[" ~> int ~ int <~ "]" ^^ { case a ~ b => Val.CasValue(a, b) } |
    "nil" ^^ { _ => Val.Nil } |
    ":timed-out" ^^ { _ => Val.Unknown } |
    int ^^ { v => Val.Value(v) }

  def entryType: Parser[EntryType] =
    ":invoke" ^^ { _ => EntryType.Invoke } |
    ":ok"     ^^ { _ => EntryType.Ok } |
    ":fail"   ^^ { _ => EntryType.Fail } |
    ":info"   ^^ { _ => EntryType.Info }

  def functionType: Parser[FunctionType] =
    ":read"  ^^ { _ => FunctionType.Read } |
    ":write" ^^ { _ => FunctionType.Write } |   
    ":cas"   ^^ { _ => FunctionType.Cas }

  val dq = '"'
  def string: Parser[String] =
    dq ~> rep(elem("string", _ != dq)) <~ dq ^^ { _.mkString("") }

  def optString: Parser[Option[String]] =
    "nil" ^^ { _ => None} | 
    string.map(Some(_))
}
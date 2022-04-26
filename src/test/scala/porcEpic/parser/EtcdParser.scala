package porcEpic
package parser

import specification.Etcd

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Lexical

import scala.io.Source

object EtcdParser extends RegexParsers {
  enum EntryType:
    case Invoke
    case Ok
    case Fail
    case Info

  enum FunctionType:
    case Read
    case Write
    case Cas

  enum Val:
    case Nil
    case Value(value: Int)
    case CasValue(expected: Int, value: Int)
    case Unknown

  opaque type ProcessId = Int
  object ProcessId {
    def apply(value: Int): ProcessId = value
  }

  case class EtcdEntry(
    process: ProcessId, 
    entry: EntryType,
    function: FunctionType,
    value: Val
  )

  def parseFile(index: String): List[Entry[Etcd.Input, Etcd.Output]] = {
    import FunctionType._
    import EntryType._
    import Etcd.State
    
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
          processToId(process) = OperationId(id)

          val input =
            (function, value) match {
              case (Read,  Val.Nil)                       => Etcd.Input.Read
              case (Write, Val.Value(value))              => Etcd.Input.Write(State(value))
              case (Cas,   Val.CasValue(expected, value)) => Etcd.Input.Cas(State(expected), State(value))
              case _ => throw new Exception(s"bogus parsing: $filename $functionType $value")
            }

          Some(
            Entry.Call[Etcd.Input, Etcd.Output](
              input,
              Time(time),
              OperationId(id),
              ClientId(process)
            )
          )


        case (EtcdEntry(process, entry, function, value), time) =>
          val output: Option[Etcd.Output] =
            (entry, function, value) match {
              case (Ok  , Read  , Val.Value(value))   => Some(Etcd.Output.Read(Some(State(value))))
              case (Ok  , Read  , Val.Nil)            => Some(Etcd.Output.Read(None))
              case (Ok  , Write , Val.Value(value))   => Some(Etcd.Output.Write(State(value)))
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
              out,
              Time(time),
              matchId,
              ClientId(process)
            )
          }

      }.toList

    // events with call but without returns
    // we dropped cas and write timeout since we don't know if the server got the packet or not
    val lastTime = events.length
    val unfinishedEvents =
      processToId.toList.zipWithIndex.map { case ((process, matchId), time) =>
        Entry.Return[Etcd.Input, Etcd.Output](
          Etcd.Output.Unknown,
          Time(lastTime + time),
          matchId,
          ClientId(process)
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
    ("INFO  jepsen.util -" ~> process) ~ entryType ~ functionType ~ value ^^ {
      case process ~ eTpe ~ fTpe ~ value =>
        EtcdEntry(process, eTpe, fTpe, value)
    }

  def process: Parser[ProcessId] = int.map(ProcessId.apply)

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
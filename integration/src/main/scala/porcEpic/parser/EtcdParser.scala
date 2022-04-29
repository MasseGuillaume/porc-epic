package porcEpic
package parser

import specification.Etcd

import cats.parse.Parser
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.Parser.{string, charsWhile, Error, Expectation}
import cats.parse.Rfc5234.{digit, wsp}

import cats.implicits.toShow


import scala.io.Source

object EtcdParser {
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
    val source = Source.fromFile(s"${build.BuildInfo.porcupine}/test_data/jepsen/etcd_${filename}.log")
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
    root.parseAll(input).fold(
      error => throw new Exception("...\n" + error.toString),
      x => x
    )

  def root: Parser[EtcdEntry] =
    (
      (
        (string("INFO  jepsen.util -") ~ wsp.rep) *> process <* wsp.rep
      ) ~ 
      (entryType <* wsp.rep) ~ 
      (functionType <* wsp.rep) ~ 
      value
    ).map {

      case (((process, eTpe), fTpe), value) =>
        EtcdEntry(process, eTpe, fTpe, value)
    }

  def process: Parser[ProcessId] = digitInt.map(ProcessId.apply)

  def digitInt : Parser[Int] = nonNegativeIntString.map(_.toInt)

  def value: Parser[Val] = 
    (string("[") *> (digitInt <* wsp.rep) ~ digitInt <* string("]")).map { case (a, b) => Val.CasValue(a, b) } |
    string("nil").map(_ => Val.Nil ) |
    string(":timed-out").map(_ => Val.Unknown ) |
    digitInt.map(v => Val.Value(v) )

  def entryType: Parser[EntryType] =
    string(":invoke").map(_ => EntryType.Invoke) |
    string(":ok").map(_ => EntryType.Ok) |
    string(":fail").map(_ => EntryType.Fail) |
    string(":info").map(_ => EntryType.Info)

  def functionType: Parser[FunctionType] =
    string(":read").map(_ => FunctionType.Read) |
    string(":write").map(_ => FunctionType.Write) |   
    string(":cas").map(_ => FunctionType.Cas)
}
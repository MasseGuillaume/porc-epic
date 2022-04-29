package porcEpic
package integration

import porcEpic.specification.EtcdTest

import specification.Etcd._
import parser.EtcdParser

class EtcdIntegrationTest extends munit.FunSuite {

  def describeOperation(operation: Operation[Input, Output]): String = 
    (operation.input, operation.output) match {
      case (Input.Read                , Output.Read(state))  => s"""read() -> ${state.getOrElse("null")}"""
      case (Input.Write(value)        , Output.Write(state)) => s"write(${state})"
      case (Input.Cas(expected, value), Output.Cas(ok))      => s"""cas($expected, $value) -> ${ if(ok) "ok" else "fail" }"""
      case (_                         , Output.Unknown)      => "unknown"
      case _                                                 => throw new Exception("invalid operation")
    }
  
  def describeState(state: Option[State]): String =
    state.fold("null")(_.toString)

  private val linearizableTests = Set(
      "2",
      "5",
      "7",
     "18",
     "25",
     "31",
     "38",
     "45",
     "48",
     "49",
     "51",
     "53",
     "56",
     "67",
     "75",
     "76",
     "80",
     "87",
     "92",
     "98",
    "100",
    "101",
    "102",
  )
  
  EtcdTest.names.foreach(name =>
    test(s"etcd $name") {
      println(s"\n-- etcd $name --")

      val entries = EtcdParser.parseFile(name)
      val (obtained, _) = specification.checkEntries(entries, verbosity = Verbosity.Debug)

      val expected = 
        if (linearizableTests.contains(name)) CheckResult.Ok
        else CheckResult.Illegal
      assertEquals(obtained, expected)
    }
  )
}

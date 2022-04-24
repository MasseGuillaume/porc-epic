package porcEpic
package integration

import org.scalatest.funsuite.AnyFunSuite

import specification.Etcd._
import parser.EtcdParser

class EtcdTest extends AnyFunSuite {

  def describeOperation(operation: Operation[Input, Output]): String =
    (operation.input, operation.output) match {
      case (Input.Read                , Output.Read(state))  => s"""read() -> ${state.getOrElse("nil")}"""
      case (Input.Write(value)        , Output.Write(state)) => s"write(${state})"
      case (Input.Cas(expected, value), Output.Cas(ok))      => s"cas($expected, $value) -> $ok"
      case (_                         , Output.Unknown)      => "unknown"
      case _                                                 => throw new Exception("invalid operation")
    }
  
  def describeState(state: Option[State]): String =
    state.fold("nil")(_.toString)

  // 102
  (0 to 102)
    .filterNot(_ == 95)
    .map(_.toString)
    .foreach(name =>

    test(s"etcd $name") {
      println(s"\n-- etcd $name --")
      val entries = EtcdParser.parseFile(name)
      val (obtained, info) = specification.checkEntries(entries, verbosity = Verbosity.Debug)

      // val data = 
      //   specification.visualize(
      //     info.get,
      //     describeOperation,
      //     describeState
      //   )
      // data.head.History.foreach(println)

      val expected = 
        if (linearizableTests.contains(name)) CheckResult.Ok
        else CheckResult.Illegal
      assert(obtained == expected)
    }
  )

  val linearizableTests = Set(
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
}

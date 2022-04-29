package porcEpic
package integration

import specification.Etcd._
import parser.EtcdParser

class EtcdTest extends munit.FunSuite {

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

  // 102
  (0 to 102)
    .filterNot(_ == 95) // this test does not exists
    .map(_.toString)
    .foreach(name =>

    test(s"etcd $name") {
      println(s"\n-- etcd $name --")

      val entries = EtcdParser.parseFile(name)
      val (obtained, _) = specification.checkEntries(entries, verbosity = Verbosity.Debug)

      // import java.nio.file._
      // import java.nio.charset.StandardCharsets
      // Files.write(
      //   Paths.get("tests"),
      //   s"etcd ${name.padTo(10, ' ')} ${leftPad((endTime - startTime).toString)(10, ' ')}\n".getBytes(StandardCharsets.UTF_8),
      //   StandardOpenOption.APPEND
      // )

      val expected = 
        if (linearizableTests.contains(name)) CheckResult.Ok
        else CheckResult.Illegal
      assertEquals(obtained, expected)
    }
  )

  test("large vizualization") {

  }

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

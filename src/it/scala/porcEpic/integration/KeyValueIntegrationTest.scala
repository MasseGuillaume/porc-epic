package porcEpic
package integration

import org.scalatest.funsuite.AnyFunSuite

import specification.KeyValue._
import parser.KeyValueParser

class KeyValueTest extends AnyFunSuite {
  
  def describeOperation(operation: Operation[Input, Output]): String = {
    operation.input match {
      case Input.Get(key)           => s"get($key) -> ${operation.output}"
      case Input.Put(key, value)    => s"put($key, $value)"
      case Input.Append(key, value) => s"append($key, $value)"
    }
  }

  List(
    "c01-bad",
    "c01-ok",
    "c10-bad",
    "c10-ok",
    "c50-ok",
    "c50-bad", // this test takes a bit more time
  ).foreach(name =>

    test(name) {

      println(s"\n-- kv $name --")

      val entries = KeyValueParser.parseFile(name)
      val startTime = System.nanoTime
      val (obtained, _) = specification.checkEntries(entries, verbosity = Verbosity.Debug)
      val endTime = System.nanoTime

      import java.nio.file._
      import java.nio.charset.StandardCharsets
      Files.write(
        Paths.get("tests"),
        s"kv ${name.padTo(10, ' ')} ${leftPad((endTime - startTime).toString)(20, ' ')}\n".getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.APPEND
      )

      val expected =
        if (name.endsWith("-bad")) CheckResult.Illegal
        else if (name.endsWith("-ok")) CheckResult.Ok
        else throw new Exception("invalid test name: " + name)

      assert(obtained == expected)
    }

  )
}

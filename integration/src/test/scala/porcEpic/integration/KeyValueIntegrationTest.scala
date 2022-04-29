package porcEpic
package integration

import porcEpic.specification.KeyValueTest
import specification.KeyValue._
import parser.KeyValueParser

class KeyValueIntegrationTest extends munit.FunSuite {
  
  def describeOperation(operation: Operation[Input, Output]): String = {
    operation.input match {
      case Input.Get(key)           => s"get($key) -> ${operation.output}"
      case Input.Put(key, value)    => s"put($key, $value)"
      case Input.Append(key, value) => s"append($key, $value)"
    }
  }

  KeyValueTest.names.foreach(name =>

    test(name) {

      println(s"\n-- kv $name --")

      val entries = KeyValueParser.parseFile(name)
      val startTime = System.nanoTime
      val (obtained, _) = specification.checkEntries(entries, verbosity = Verbosity.Debug)
      val endTime = System.nanoTime

      val expected =
        if (name.endsWith("-bad")) CheckResult.Illegal
        else if (name.endsWith("-ok")) CheckResult.Ok
        else throw new Exception("invalid test name: " + name)

      assertEquals(obtained, expected)
    }

  )
}

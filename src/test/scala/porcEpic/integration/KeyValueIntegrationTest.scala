package porcEpic
package integration

import org.scalatest.funsuite.AnyFunSuite

import specification.KeyValue._
import parser.KeyValueParser

class KeyValueTest extends AnyFunSuite {
  

  List(
    "c01-bad",
    "c01-ok",
    "c10-bad",
    "c10-ok",
    "c50-bad",
    "c50-ok",
  ).foreach(name =>

    test(name) {
      val entries = KeyValueParser.parseFile(name)
      val (obtained, _) = specification.checkEntries(entries)

      val expected =
        if (name.endsWith("-bad")) CheckResult.Illegal
        else if (name.endsWith("-ok")) CheckResult.Ok
        else throw new Exception("invalid test name: " + name)

      assert(obtained == expected)
    }
  )
}

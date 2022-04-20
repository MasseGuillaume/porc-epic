package porcEpic
package integration

import org.scalatest.funsuite.AnyFunSuite

import specification.Etcd._
import parser.EtcdParser

class EtcdTest extends AnyFunSuite {
  
  (0 to 102).map(_.toString).foreach(name =>
    test(name) {
      println(s"\n-- $name --")
      val entries = EtcdParser.parseFile(name)
      val (obtained, _) = specification.checkEntries(entries, verbosity = Verbosity.Debug)

      val expected =
        if (name.endsWith("-bad")) CheckResult.Illegal
        else if (name.endsWith("-ok")) CheckResult.Ok
        else throw new Exception("invalid test name: " + name)

      assert(obtained == expected)
    }
  )
}

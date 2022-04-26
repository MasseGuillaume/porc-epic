package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite
import porcEpic.specification.Etcd

class EtcdParserTest extends AnyFunSuite {
  import parser.EtcdParser
  import EtcdParser._
  import EntryType._
  import FunctionType._
  import Val._

  // read
  parseTest("INFO  jepsen.util - 0 :invoke :read nil"       , EtcdEntry(ProcessId(0), Invoke, Read, Nil))
  parseTest("INFO  jepsen.util - 0 :ok :read nil"           , EtcdEntry(ProcessId(0), Ok,     Read, Nil))
  parseTest("INFO  jepsen.util - 0 :ok :read 3"             , EtcdEntry(ProcessId(0), Ok,     Read,  Value(3)))
  parseTest("INFO  jepsen.util - 3 :fail :read :timed-out"  , EtcdEntry(ProcessId(3), Fail,   Read,  Unknown))

  // write
  parseTest("INFO  jepsen.util - 2 :invoke :write 4"        , EtcdEntry(ProcessId(2), Invoke, Write, Value(4)))
  parseTest("INFO  jepsen.util - 4 :ok :write 3"            , EtcdEntry(ProcessId(4), Ok,     Write, Value(3)))
  parseTest("INFO  jepsen.util - 4 :info :write :timed-out" , EtcdEntry(ProcessId(4), Info,   Write, Unknown))

  // cas
  parseTest("INFO  jepsen.util - 3 :invoke :cas [3 4]"      , EtcdEntry(ProcessId(3), Invoke, Cas, CasValue(3, 4)))
  parseTest("INFO  jepsen.util - 2 :ok :cas [3 0]"          , EtcdEntry(ProcessId(2), Ok,     Cas, CasValue(3, 0)))
  parseTest("INFO  jepsen.util - 1 :fail :cas [0 3]"        , EtcdEntry(ProcessId(1), Fail,   Cas, CasValue(0, 3)))
  parseTest("INFO  jepsen.util - 1 :info :cas :timed-out"   , EtcdEntry(ProcessId(1), Info,   Cas, Unknown))

  def parseTest(input: String, expected: EtcdParser.EtcdEntry): Unit =
    test(input) {
      val obtained = parse(input)
      assert(obtained == expected)
    }
}

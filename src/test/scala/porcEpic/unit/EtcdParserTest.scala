package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite
import porcEpic.specification.Etcd

class EtcdParserTest extends AnyFunSuite {
  import parser.EtcdParser
  import EtcdParser._

  // read
  parseTest(
    "INFO  jepsen.util - 0 :invoke :read nil", 
    EtcdEntry(process = 0, entry = EntryType.Invoke, function = FunctionType.Read,  value = Val.Nil)
  )

  parseTest(
    "INFO  jepsen.util - 0 :ok :read nil", 
    EtcdEntry(process = 0, entry = EntryType.Ok,     function = FunctionType.Read,  value = Val.Nil)
  )

  parseTest(
    "INFO  jepsen.util - 0 :ok :read 3", 
    EtcdEntry(process = 0, entry = EntryType.Ok,     function = FunctionType.Read,  value = Val.Value(3))
  )

  parseTest(
    "INFO  jepsen.util - 3   :fail   :read   :timed-out", 
    EtcdEntry(process = 3, entry = EntryType.Fail,   function = FunctionType.Read,  value = Val.Unknown)
  )


  // write
  parseTest(
    "INFO  jepsen.util - 2  :invoke :write  4", 
    EtcdEntry(process = 2, entry = EntryType.Invoke, function = FunctionType.Write, value = Val.Value(4))
  )

  parseTest(
    "INFO  jepsen.util - 4 :ok :write  3", 
    EtcdEntry(process = 4, entry = EntryType.Ok,     function = FunctionType.Write, value = Val.Value(3))
  )

  parseTest(
    "INFO  jepsen.util - 4 :info :write  :timed-out", 
    EtcdEntry(process = 4, entry = EntryType.Info,   function = FunctionType.Write, value = Val.Unknown)
  )


  // cas
  parseTest(
    "INFO  jepsen.util - 3 :invoke :cas  [3 4]",
    EtcdEntry(process = 3, entry = EntryType.Invoke, function = FunctionType.Cas,   value = Val.CasValue(3, 4))
  )

  parseTest(
    "INFO  jepsen.util - 2 :ok :cas  [3 0]",
    EtcdEntry(process = 2, entry = EntryType.Ok,     function = FunctionType.Cas,   value = Val.CasValue(3, 0))
  )

  parseTest(
    "INFO  jepsen.util - 1 :fail :cas  [0 3]",
    EtcdEntry(process = 1, entry = EntryType.Fail,   function = FunctionType.Cas,   value = Val.CasValue(0, 3))
  )

  parseTest(
    "INFO  jepsen.util - 1 :info :cas  :timed-out", 
    EtcdEntry(process = 1, entry = EntryType.Info,   function = FunctionType.Cas,   value = Val.Unknown)
  )

  def parseTest(input: String, expected: EtcdParser.EtcdEntry): Unit =
    test(input) {
      val obtained = parse(input)
      assert(obtained == expected)
    }
}

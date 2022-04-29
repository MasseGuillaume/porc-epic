package porcEpic

import specification.{KeyValue, KeyValueTest}
import specification.KeyValue.specification
import porcEpic.parser.KeyValueParser

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class KeyValueBench {

  val parsedEntries = scala.collection.mutable.Map.empty[String, List[Entry[KeyValue.Input, KeyValue.Output]]]

  KeyValueTest.names.foreach{name =>
    parsedEntries(name) = KeyValueParser.parseFile(name)
  }

  def test(n: String): Unit = {
    val entries = parsedEntries(n)
    specification.checkEntries(entries)
  }

  @Benchmark def kv_c01_bad: Unit = test("c01-bad")
  @Benchmark def kv_c01_ok: Unit  = test("c01-ok")
  // @Benchmark def kv_c10_bad: Unit = test("c10-bad")
  // @Benchmark def kv_c10_ok: Unit  = test("c10-ok")
  // @Benchmark def kv_c50_ok: Unit  = test("c50-ok")
  // @Benchmark def kv_c50_bad: Unit = test("c50-bad")
}



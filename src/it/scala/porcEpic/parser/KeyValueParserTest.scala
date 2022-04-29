package porcEpic
package unit

class KeyValueParserTest extends munit.FunSuite {

  import parser.KeyValueParser
  import KeyValueParser._
  import EntryType._
  import FunctionType._

  def parse(input: String): KeyValueEntry = KeyValueParser(input)

  parseTest(
    """{:process 0, :type :invoke, :f :get, :key "7", :value nil}""",
    KeyValueEntry(0, Invoke, Get, "7", None)
  )

  parseTest(
    """{:process 0, :type :ok, :f :get, :key "7", :value "x 0 4 y"}""",
    KeyValueEntry(0, Ok, Get, "7", Some("x 0 4 y"))
  )

  parseTest(
    """{:process 1, :type :invoke, :f :put, :key "8", :value "x 1 0 y"}""",
    KeyValueEntry(1, Invoke, Put, "8", Some("x 1 0 y"))
  )

  parseTest(
    """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
    KeyValueEntry(0, Invoke, Append, "0", Some("x 0 0 y"))
  )

  def parseTest(input: String, expected: KeyValueEntry)(implicit
      loc: munit.Location
  ): Unit =
    test(input) {
      val obtained = parse(input)
      assertEquals(obtained, expected)
    }
}

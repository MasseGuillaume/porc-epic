package porcEpic
package unit

import org.scalatest.funsuite.AnyFunSuite

class KeyValueParserTest extends AnyFunSuite {

  import parser.KeyValueParser
  import KeyValueParser._

  def parse(input: String): KeyValueEntry = KeyValueParser(input)

  test("parse get invoke") {
    val input = """{:process 0, :type :invoke, :f :get, :key "7", :value nil}"""
    val obtained = parse(input)
    val expected = KeyValueEntry(
      process = 0, 
      `type` = EntryType.Invoke,
      f = FunctionType.Get,
      key = "7",
      value = None
    )
    assert(obtained == expected)
  }

  test("parse get return") {
    val input = """{:process 0, :type :ok, :f :get, :key "7", :value "x 0 4 y"}"""
    val obtained = parse(input)
    val expected = KeyValueEntry(
      process = 0, 
      `type` = EntryType.Ok,
      f = FunctionType.Get,
      key = "7",
      value = Some("x 0 4 y")
    )
    assert(obtained == expected)
  }

  test("parse put") {
    val input = """{:process 1, :type :invoke, :f :put, :key "8", :value "x 1 0 y"}"""
    val obtained = parse(input)
    val expected = KeyValueEntry(
      process = 1, 
      `type` = EntryType.Invoke,
      f = FunctionType.Put,
      key = "8",
      value = Some("x 1 0 y")
    )
    assert(obtained == expected)
  }

  test("parse append") {
    val input = """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}"""
    val obtained = parse(input)
    val expected = KeyValueEntry(
      process = 0, 
      `type` = EntryType.Invoke,
      f = FunctionType.Append,
      key = "0",
      value = Some("x 0 0 y")
    )
    assert(obtained == expected)
  }
}

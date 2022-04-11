package porcEpic
package jepsen

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.Lexical

import org.scalatest.funsuite.AnyFunSuite

object KeyValueParser extends RegexParsers {
  enum EntryType:
    case Invoke extends EntryType
    case Ok extends EntryType

  enum FunctionType:
    case Get extends FunctionType
    case Put extends FunctionType
    case Append extends FunctionType

  case class KeyValueEntry(
    process: Int, 
    `type`: EntryType,
    f: FunctionType,
    key: String,
    value: Option[String]
  )

  def apply(input: String): KeyValueEntry = 
    parseAll(root, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        val offset = failure.next.offset
        val carret = (1 to offset).map(_ => " ").mkString("") + "^"
        throw new Exception(
          s"""|${failure.msg}
              |$input
              |$carret""".stripMargin
        )
    }

  def root: Parser[KeyValueEntry] =
    "{" ~> 
       (":process" ~> int <~ ",") ~
       (":type" ~> entryType <~ ",") ~
       (":f" ~> functionType <~ ",") ~
       (":key " ~> string <~ ",") ~
       (":value " ~> optString) <~
    "}" ^^ {
      case process ~ tpe ~ f ~ key ~ value =>
        KeyValueEntry(process, tpe, f, key, value)
    }

  def int: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def entryType: Parser[EntryType] =
    ":invoke" ^^ { _ => EntryType.Invoke } |
    ":ok"     ^^ { _ => EntryType.Ok }

  def functionType: Parser[FunctionType] =
    ":get"    ^^ { _ => FunctionType.Get } |
    ":put"    ^^ { _ => FunctionType.Put } |   
    ":append" ^^ { _ => FunctionType.Append }

  val dq = '"'
  def string: Parser[String] =
    dq ~> rep(elem("string", _ != dq)) <~ dq ^^ { _.mkString("") }

  def optString: Parser[Option[String]] =
    "nil" ^^ { _ => None} | 
    string.map(Some(_))
}

class KeyValueParseTest extends AnyFunSuite {

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

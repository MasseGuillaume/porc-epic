package porcEpic
package parser

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.Lexical

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
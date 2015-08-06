package PiParser {

  import scala.util.parsing.combinator._

  sealed abstract class Token
  case class Identifier(text: String) extends Token
  case class Integer(value: Int) extends Token

  object Lexer extends RegexParsers {
    def ident: Parser[Token] =
      """[a-z]+""".r ^^ { s => Identifier(s) }
    def integer: Parser[Token] =
      """0|[1-9][0-9]*""".r ^^ { s => Integer(s.toInt) }
  }
}

package PiParser {

  import scala.util.parsing.combinator._
  import Syntax.Syntax._

  sealed abstract class Token
  case class TkVar(text: String) extends Token
  case class TkChan(text: String) extends Token
  case class TkInt(value: Int) extends Token
  case class TkBool(value: Boolean) extends Token
  case class TkBinOp(op: BinOp) extends Token
  case object TkVBar extends Token
  case object TkBang extends Token
  case object TkQMark extends Token
  case object TkNew extends Token
  case object TkDot extends Token
  case object TkLet extends Token
  case object TkEqu extends Token
  case object TkIf extends Token
  case object TkThen extends Token
  case object TkElse extends Token
  case object TkEnd extends Token

  object Lexer extends RegexParsers {
    def _var: Parser[Token] =
      """(_|[a-zA-Z])((_|[a-zA-Z0-9])*)""".r ^^ { s => TkVar(s) }
    def _chan: Parser[Token] =
      """@(_|[a-zA-Z])((_|[a-zA-Z0-9])*)""".r ^^ { s => TkChan(s) }
    def _int: Parser[Token] =
      """0|[1-9][0-9]*""".r ^^ { s => TkInt(s.toInt) }

    def _true: Parser[Token] =
      """true""".r ^^ { _ => TkBool(true) }
    def _false: Parser[Token] =
      """false""".r ^^ { _ => TkBool(false) }

    def _add: Parser[Token] =
      """\+""".r ^^ { _ => TkBinOp(Add) }
    def _sub: Parser[Token] =
      """\-""".r ^^ { _ => TkBinOp(Sub) }
    def _mul: Parser[Token] =
      """\*""".r ^^ { _ => TkBinOp(Mul) }
    def _div: Parser[Token] =
      """\/""".r ^^ { _ => TkBinOp(Div) }
    def _mod: Parser[Token] =
      """\%""".r ^^ { _ => TkBinOp(Mod) }

    def _equal: Parser[Token] =
      """==""".r ^^ { _ => TkBinOp(Equal) }
    def _notequal: Parser[Token] =
      """!=""".r ^^ { _ => TkBinOp(NotEqual) }

    def _less: Parser[Token] =
      """<""".r ^^ { _ => TkBinOp(Less) }
    def _lesseq: Parser[Token] =
      """<=""".r ^^ { _ => TkBinOp(LessEq) }
    def _greater: Parser[Token] =
      """>""".r ^^ { _ => TkBinOp(Greater) }
    def _greatereq: Parser[Token] =
      """>=""".r ^^ { _ => TkBinOp(GreaterEq) }

    def _and: Parser[Token] =
      """&&""".r ^^ { _ => TkBinOp(And) }
    def _or: Parser[Token] =
      """\|\|""".r ^^ { _ => TkBinOp(Or) }

    def _vbar: Parser[Token] =
      """\|""".r ^^ { _ => TkVBar }
    def _bang: Parser[Token] =
      """\!""".r ^^ { _ => TkBang }
    def _qmark: Parser[Token] =
      """\?""".r ^^ { _ => TkQMark }
    def _new: Parser[Token] =
      """new""".r ^^ { _ => TkNew }
    def _dot: Parser[Token] =
      """\.""".r ^^ { _ => TkDot }
    def _let: Parser[Token] =
      """let""".r ^^ { _ => TkLet }
    def _equ: Parser[Token] =
      """=""".r ^^ { _ => TkEqu }
    def _if: Parser[Token] =
      """if""".r ^^ { _ => TkIf }
    def _then: Parser[Token] =
      """then""".r ^^ { _ => TkThen }
    def _else: Parser[Token] =
      """else""".r ^^ { _ => TkElse }
    def _end: Parser[Token] =
      """end""".r ^^ { _ => TkEnd }
  }
}

package newparser

import syntax._
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

class Parser extends RegexParsers with PackratParsers {

  def name: Parser [ Name ] = """[a-z]+""".r ^^ { _ => Name ( ??? ) }

  lazy val proc: PackratParser [ Proc ] = par | end | snd | rcv | res

  lazy val par: PackratParser [ Proc ] = proc ~ "|" ~ proc ^^ {
    case p ~ _ ~ q => Parallel ( p , q )
  }

  def snd: Parser [ Proc ] =
    "send" ~ name ~ ":" ~ name ~ "." ~ proc ^^ {
      case _ ~ c ~ _ ~ m ~ _ ~ p => Send ( Variable ( c ) , Variable ( m ) , p )
    }

  def rcv: Parser [ Proc ] =
    "receive" ~ name ~ ":" ~ name ~ "." ~ proc ^^ {
      case _ ~ c ~ _ ~ b ~ _ ~ p => Receive ( false , Variable ( c ) , b , p )
    }

  def res: Parser [ Proc ] = "new" ~ name ~ "." ~ proc ^^ {
    case _ ~ n ~ _ ~ p => New ( n , p )
  }

  def end: Parser [ Proc ] = "end" ^^ { _ => End }

  lazy val exp: PackratParser [ Exp ] =
    variable | intLiteral | trueLiteral | falseLiteral | chanLiteral | pair |
    binExp | unExp | "(" ~> exp <~ ")"

  def variable: Parser [ Exp ] = name ^^ { Variable ( _ ) }

  def intLiteral: Parser [ Exp ] = """(0|[1-9]\d*)""".r ^^ {
    s => IntLiteral ( s.toInt )
  }

  def trueLiteral: Parser [ Exp ] = "true" ^^ { _ => BoolLiteral ( true ) }

  def falseLiteral: Parser [ Exp ] = "false" ^^ { _ => BoolLiteral ( false ) }

  def chanLiteral: Parser [ Exp ] = """\$[a-z]+""".r ^^ {
    _ => ChanLiteral ( Name ( ??? ) )
  }

  def pair: Parser [ Exp ] = "{" ~ exp ~ "," ~ exp ~ "}" ^^ {
    case _ ~ l ~ _ ~ r ~ _ => Pair ( l , r )
  }

  lazy val binExp: PackratParser [ Exp ] = exp ~ binOpTy ~ exp ^^ {
    case l ~ op ~ r => BinExp ( op , l , r )
  }

  def unExp: Parser [ Exp ] = unOpTy ~ exp ^^ {
    case op ~ e => UnExp ( op , e )
  }

  def uMinus: Parser [ Exp ] = "-" ~ exp ^^ {
    case _ ~ e => BinExp ( Sub , IntLiteral ( 0 ) , e )
  }

  def binOpTy: Parser [ BinOp ] =
    "+"  ^^ { _ => Add       } |
    "-"  ^^ { _ => Sub       } |
    "*"  ^^ { _ => Mul       } |
    "/"  ^^ { _ => Div       } |
    "%"  ^^ { _ => Mod       } |
    "==" ^^ { _ => Equal     } |
    "!=" ^^ { _ => NotEqual  } |
    "<"  ^^ { _ => Less      } |
    "<=" ^^ { _ => LessEq    } |
    ">"  ^^ { _ => Greater   } |
    ">=" ^^ { _ => GreaterEq } |
    "&&" ^^ { _ => And       } |
    "||" ^^ { _ => Or        }

  def unOpTy: Parser [ UnOp ] =
    "!"  ^^ { _ => Not    } |
    "<-" ^^ { _ => PLeft  } |
    "->" ^^ { _ => PRight }
}

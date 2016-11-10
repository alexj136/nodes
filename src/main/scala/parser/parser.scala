package parser

/** Implements a parser for the nodes language using scala's parser combinators.
 *  Parsing is subdivided into separate lexing, postlexing and parsing phases.
 *  - Lexing converts a String of nodes code into a List [ PreToken ]. PreTokens
 *  are tokens where identifiers, ints and such carry the text that was lexed
 *  to produce them.
 *  - The postlexing step converts PreTokens into PostTokens, changing the
 *  representation of names from Strings to Integers, providing a map between
 *  string and integer names for printing purposes.
 *  - Parsing converts a List [ PostToken ] into a syntax.Proc.
 */

import syntax._
import scala.io.Source
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.collection.immutable.PagedSeq

/** Responsible for converting a List [ PostToken ] into a syntax.Proc via the
 *  ... method.
 */

sealed abstract class LexerParserError ( row: Int , col: Int , msg: String )
case class LexerError  ( row: Int , col: Int , msg: String )
  extends LexerParserError ( row , col , msg )
case class ParserError ( row: Int , col: Int , msg: String )
  extends LexerParserError ( row , col , msg )

object lexAndParse {

  def apply [ T ] (
    production: Parser.Parser [ T ] ,
    input: Source
  ): Either [ LexerParserError , ( Map [ String , Name ] , Name , T ) ] =
    for {
      lexed  <- Lexer  ( input                       ).right
      parsed <- Parser ( production , lexed._3       ).right
    } yield ( lexed._1 , lexed._2 , parsed )
}

object Parser extends Parsers {

  def apply [ T ] (
    production: Parser [ T ] ,
    input: List [ PostToken ]
  ): Either [ ParserError , T ] =
    production ( new TokenReader ( input ) ) match {
      case Success   ( prc , rest ) => Right ( prc                 )
      case NoSuccess ( msg , rest ) =>
        Left ( ParserError ( rest.pos.line , rest.pos.column , msg ) )
    }


  /** Take a SyntaxElement and two PostTokens, assign the left source position 
   *  of the SyntaxElement as the position of the first token, and the right
   *  source position of the SyntaxElement as the position of the second token.
   *  Return the SyntaxElement with reassigned positions.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: PostToken ,
  r: PostToken ): T = {
    elem.setInfo ( SrcPosInfo ( ( l.pos.line , l.pos.column ) ,
      ( r.pos.line, r.pos.column ) ) )
    elem
  }

  /** As above, but using positions taken from SyntaxElements as opposed to
   *  PostTokens.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: SyntaxElement ,
  r: SyntaxElement ): T = ( l.info , r.info ) match {
    case ( SrcPosInfo ( ll , lr ) , SrcPosInfo ( rl , rr ) ) =>
      elem.setInfo ( SrcPosInfo ( ll , rr ) ) ; elem
    case _ => elem.setInfo ( NoInfo ) ; elem
  }

  /** As above, where the first position is from a PostToken and the second from
   *  a SyntaxElement.
   */
  def putPos [ T <: SyntaxElement ] ( elem: T , l: PostToken ,
  r: SyntaxElement ): T = r.info match {
    case SrcPosInfo ( rl , rr ) =>
      elem.setInfo ( SrcPosInfo ( ( l.pos.line , l.pos.column ) , rr ) ) ; elem
    case _ => elem.setInfo ( NoInfo ) ; elem
  }

  override type Elem = PostToken

  def name: Parser [ Name ] = accept ( "POSTIDENT" , {
    case p @ POSTIDENT ( n ) => putPos ( n , p , p )
  } )

  def proc: Parser [ Proc ] = phrase ( seq )

  def seq: Parser [ Proc ] = end | snd | rcv | srv | res | let | ite | par

  def par: Parser [ Proc ] = LSQUARE() ~ rep1sep ( seq , BAR() ) ~ RSQUARE() ^^ {
    case l ~ p ~ r => putPos ( Proc fromList p , l , r )
  }

  def snd: Parser [ Proc ] = SEND() ~ exp ~ COLON() ~ exp ~ DOT() ~ seq ^^ {
    case s ~ c ~ _ ~ m ~ d ~ p => putPos ( Send ( c , m , p ) , s , d )
  }

  def rcv: Parser [ Proc ] = RECEIVE() ~ exp ~ COLON() ~ name ~ DOT() ~ seq ^^ {
    case r ~ c ~ _ ~ b ~ d ~ p =>
      putPos ( Receive ( false , c , b , p ) , r , d )
  }

  def srv: Parser [ Proc ] = SERVER() ~ exp ~ COLON() ~ name ~ DOT() ~ seq ^^ {
    case s ~ c ~ _ ~ b ~ d ~ p =>
      putPos ( Receive ( true , c , b , p ) , s , d )
  }

  def res: Parser [ Proc ] = NEW() ~ name ~ DOT() ~ seq ^^ {
    case nu ~ n ~ d ~ p => putPos ( New ( n , p ) , nu , d )
  }

  def let: Parser [ Proc ] = LET() ~ name ~ EQUAL() ~ exp ~ DOT() ~ seq ^^ {
    case l ~ n ~ _ ~ e ~ d ~ p => putPos ( LetIn ( n , e , p ) , l , d )
  }

  def ite: Parser [ Proc ] =
    IF() ~ exp ~ THEN() ~ seq ~ ELSE() ~ seq ~ ENDIF() ^^ {
      case i ~ e ~ _ ~ p ~ _ ~ q ~ d =>
        putPos ( IfThenElse ( e , p , q ) , i , d )
    }

  def end: Parser [ Proc ] = END() ^^ { case end => putPos ( End , end , end ) }

  /**
   * Combinator parsers for expressions. The only left-recursive production in
   * the basic expression grammar for this language is the binary expression
   * production, so we add an extra expNoBinExp production to remove the left
   * recursion.
   */

  def exp: Parser [ Exp ] = binExp | expNoBinExp

  def expNoBinExp: Parser [ Exp ] = variable | intLiteral | trueLiteral |
    falseLiteral | chanLiteral | pair | unExp | parens | emptyList | list

  def binExp: Parser [ Exp ] = expNoBinExp ~ binOpTy ~ exp ^^ {
    case l ~ op ~ r => putPos ( BinExp ( op , l , r ) , l , r )
  }

  def parens: Parser [ Exp ] = LPAREN() ~ exp ~ RPAREN() ^^ {
    case l ~ e ~ r => putPos ( e , l , r )
  }

  def variable: Parser [ Exp ] = name ^^ {
    case n => putPos ( Variable ( n ) , n , n )
  }

  def intLiteral: Parser [ Exp ] = accept ( "POSTINT" , {
    case p @ POSTINT ( i ) => putPos ( IntLiteral ( i ) , p , p )
  } )

  def trueLiteral: Parser [ Exp ] = TRUE() ^^ {
    case t => putPos ( BoolLiteral ( true ) , t , t )
  }

  def falseLiteral: Parser [ Exp ] = FALSE() ^^ {
    case f => putPos ( BoolLiteral ( false ) , f , f )
  }

  def chanLiteral: Parser [ Exp ] = accept ( "POSTCHAN" , {
    case p @ POSTCHAN ( c ) => putPos ( ChanLiteral ( c ) , p , p )
  } )

  def pair: Parser [ Exp ] = LCURLY() ~ exp ~ COMMA() ~ exp ~ RCURLY() ^^ {
    case lp ~ l ~ _ ~ r ~ rp => putPos ( Pair ( l , r ) , lp , rp )
  }

  def unExp: Parser [ Exp ] = unOpTy ~ exp ^^ {
    case op ~ e => putPos ( UnExp ( op , e ) , op , e )
  }

  def uMinus: Parser [ Exp ] = DASH() ~ exp ^^ {
    case d ~ e => putPos ( BinExp ( Sub , IntLiteral ( 0 ) , e ) , d , e )
  }

  def emptyList: Parser [ Exp ] = LSQUARE() ~ RSQUARE() ^^ {
    case l ~ r => putPos ( ListExp ( List.empty ) , l , r )
  }

  def list: Parser [ Exp ] =
    LSQUARE() ~ rep1sep ( exp , COMMA() ) ~ RSQUARE() ^^ {
      case l ~ es ~ r => putPos ( ListExp ( es ) , l , r )
    }

  def binOpTy: Parser [ BinOp ] =
    PLUS   ( ) ^^ { case t  => putPos ( Add       , t , t ) } |
    DASH   ( ) ^^ { case t  => putPos ( Sub       , t , t ) } |
    STAR   ( ) ^^ { case t  => putPos ( Mul       , t , t ) } |
    FSLASH ( ) ^^ { case t  => putPos ( Div       , t , t ) } |
    PERC   ( ) ^^ { case t  => putPos ( Mod       , t , t ) } |
    EQEQ   ( ) ^^ { case t  => putPos ( Equal     , t , t ) } |
    NEQ    ( ) ^^ { case t  => putPos ( NotEqual  , t , t ) } |
    LESS   ( ) ^^ { case t  => putPos ( Less      , t , t ) } |
    LESSEQ ( ) ^^ { case t  => putPos ( LessEq    , t , t ) } |
    GRTR   ( ) ^^ { case t  => putPos ( Greater   , t , t ) } |
    GRTREQ ( ) ^^ { case t  => putPos ( GreaterEq , t , t ) } |
    AND    ( ) ^^ { case t  => putPos ( And       , t , t ) } |
    OR     ( ) ^^ { case t  => putPos ( Or        , t , t ) } |
    CONS   ( ) ^^ { case t  => putPos ( Cons      , t , t ) }

  def unOpTy: Parser [ UnOp ] =
    BANG   ( ) ^^ { case t  => putPos ( Not    , t , t ) } |
    LARROW ( ) ^^ { case t  => putPos ( PLeft  , t , t ) } |
    RARROW ( ) ^^ { case t  => putPos ( PRight , t , t ) } |
    QMARK  ( ) ^^ { case t  => putPos ( Empty  , t , t ) } |
    HEAD   ( ) ^^ { case t  => putPos ( Head   , t , t ) } |
    TAIL   ( ) ^^ { case t  => putPos ( Tail   , t , t ) }
}

/** Reader for Tokens used to feed a List [ PostToken ] into the Parser.
 */
class TokenReader ( tokens: List [ PostToken ] ) extends Reader [ PostToken ] {
  override def first : PostToken = tokens.head
  override def atEnd : Boolean = tokens.isEmpty
  override def pos   : Position = NoPosition
  override def rest  : Reader [ PostToken ] = new TokenReader( tokens.tail )
}

/** The lexer - provides the lex method which convers a String into a
 *  List [ PreToken ].
 */
object Lexer extends RegexParsers {

  def apply ( input: Source ): Either
    [ LexerError
    , ( Map [ String , Name ] , Name , List [ PostToken ] )
    ] = lex ( new PagedSeqReader ( PagedSeq.fromSource ( input ) ) ) match {
      case Success   ( tks , rest ) => Right ( PreToken.postLexAll ( tks ) )
      case NoSuccess ( msg , rest ) =>
        Left ( LexerError ( rest.pos.line , rest.pos.column , msg ) )
    }

  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f\n]+""".r

  def lex : Parser [ List [ PreToken ] ] = phrase ( rep1 (
    positioned { """[a-z_]+""".r      ^^ { PREIDENT ( _ )   } } |||
    positioned { """\$[a-z_]+""".r    ^^ { PRECHAN  ( _ )   } } |||
    positioned { """(0|[1-9]\d*)""".r ^^ { PREINT   ( _ )   } } |||
    positioned { "!"                  ^^ { _ => BANG    ( ) } } |||
    positioned { "*"                  ^^ { _ => STAR    ( ) } } |||
    positioned { "."                  ^^ { _ => DOT     ( ) } } |||
    positioned { ":"                  ^^ { _ => COLON   ( ) } } |||
    positioned { "let"                ^^ { _ => LET     ( ) } } |||
    positioned { "new"                ^^ { _ => NEW     ( ) } } |||
    positioned { "if"                 ^^ { _ => IF      ( ) } } |||
    positioned { "then"               ^^ { _ => THEN    ( ) } } |||
    positioned { "else"               ^^ { _ => ELSE    ( ) } } |||
    positioned { "endif"              ^^ { _ => ENDIF   ( ) } } |||
    positioned { "send"               ^^ { _ => SEND    ( ) } } |||
    positioned { "receive"            ^^ { _ => RECEIVE ( ) } } |||
    positioned { "server"             ^^ { _ => SERVER  ( ) } } |||
    positioned { "|"                  ^^ { _ => BAR     ( ) } } |||
    positioned { "end"                ^^ { _ => END     ( ) } } |||
    positioned { "("                  ^^ { _ => LPAREN  ( ) } } |||
    positioned { ")"                  ^^ { _ => RPAREN  ( ) } } |||
    positioned { "{"                  ^^ { _ => LCURLY  ( ) } } |||
    positioned { "}"                  ^^ { _ => RCURLY  ( ) } } |||
    positioned { "["                  ^^ { _ => LSQUARE ( ) } } |||
    positioned { "]"                  ^^ { _ => RSQUARE ( ) } } |||
    positioned { ","                  ^^ { _ => COMMA   ( ) } } |||
    positioned { "<-"                 ^^ { _ => LARROW  ( ) } } |||
    positioned { "->"                 ^^ { _ => RARROW  ( ) } } |||
    positioned { "true"               ^^ { _ => TRUE    ( ) } } |||
    positioned { "false"              ^^ { _ => FALSE   ( ) } } |||
    positioned { "+"                  ^^ { _ => PLUS    ( ) } } |||
    positioned { "-"                  ^^ { _ => DASH    ( ) } } |||
    positioned { "/"                  ^^ { _ => FSLASH  ( ) } } |||
    positioned { "%"                  ^^ { _ => PERC    ( ) } } |||
    positioned { "="                  ^^ { _ => EQUAL   ( ) } } |||
    positioned { "=="                 ^^ { _ => EQEQ    ( ) } } |||
    positioned { "!="                 ^^ { _ => NEQ     ( ) } } |||
    positioned { "<"                  ^^ { _ => LESS    ( ) } } |||
    positioned { "<="                 ^^ { _ => LESSEQ  ( ) } } |||
    positioned { ">"                  ^^ { _ => GRTR    ( ) } } |||
    positioned { ">="                 ^^ { _ => GRTREQ  ( ) } } |||
    positioned { "&&"                 ^^ { _ => AND     ( ) } } |||
    positioned { "||"                 ^^ { _ => OR      ( ) } } |||
    positioned { "::"                 ^^ { _ => CONS    ( ) } } |||
    positioned { "?"                  ^^ { _ => QMARK   ( ) } } |||
    positioned { "*--"                ^^ { _ => HEAD    ( ) } } |||
    positioned { "-**"                ^^ { _ => TAIL    ( ) } } ) )
}

/** Below we have the token classes. We have PreTokens and PostTokens to
 *  represent tokens before and after the postlexing of lexed information.
 *  KeyWdTokens are both Pre and Post tokens because they require no postlexing.
 *  Tokens that contain information are divided into separate Pre and Post
 *  InfoToken classes as they require postlexing.
 */

sealed trait Token extends Positional
sealed abstract class PreToken extends Token {
  def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken )
}
object PreToken {
  def postLexAll (
    tokens: List [ PreToken ]
  ): ( Map [ String , Name ] , Name , List [ PostToken ] ) = {
    var nameMap  : Map [ String , Name ] = Map.empty
    var nextName : Name                  = Name ( 0 )
    var done     : List [ PostToken ]    = List ( )
    var todo     : List [ PreToken  ]    = tokens
    while ( ! todo.isEmpty ) {
      val todoHeadProcessed = todo.head.postLex ( nameMap , nextName )
      nameMap  = todoHeadProcessed._1
      nextName = todoHeadProcessed._2
      done     = done ++ List ( todoHeadProcessed._3 )
      todo     = todo.tail
    }
    ( nameMap , nextName , done )
  }
}
sealed abstract class PreInfoToken ( text: String ) extends PreToken

case class PREIDENT ( text: String ) extends PreInfoToken ( text ) {
  /** Processing and IDENT consists of looking up the name map to see if the
   *  string name already has an integer representation. If so, we use that
   *  integer. If not, we choose a new integer (via Name.next) as the
   *  representation and add that mapping to the map.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    nameMap get this.text match {
      case Some ( name ) => ( nameMap ,
        nextName      , POSTIDENT ( name     ).setPos ( this.pos ) )
      case None          => ( nameMap + ( ( this.text , nextName ) ) ,
        nextName.next , POSTIDENT ( nextName ).setPos ( this.pos ) )
    }
}
case class PRECHAN  ( text: String ) extends PreInfoToken ( text ) {
  /** Processing a CHAN is the same as with IDENTs.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    nameMap get this.text match {
      case Some ( name ) => ( nameMap ,
        nextName      , POSTCHAN ( name     ).setPos ( this.pos ) )
      case None          => ( nameMap + ( ( this.text , nextName ) ) ,
        nextName.next , POSTCHAN ( nextName ).setPos ( this.pos ) )
    }
}
case class PREINT   ( text: String ) extends PreInfoToken ( text ) {
  /** To postlex an INT, we simply have to .toInt the string.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    ( nameMap , nextName , POSTINT ( this.text.toInt ).setPos ( this.pos ) )
}

sealed trait PostToken extends Token
sealed trait PostInfoToken extends PostToken

case class POSTIDENT ( name: Name ) extends PostInfoToken
case class POSTCHAN  ( name: Name ) extends PostInfoToken
case class POSTINT   ( num:  Int  ) extends PostInfoToken

sealed abstract class KeyWdToken extends PreToken with PostToken {
  val text: String
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    ( nameMap , nextName , this )
}

case class BANG    ( ) extends KeyWdToken { val text: String = "!"       }
case class STAR    ( ) extends KeyWdToken { val text: String = "*"       }
case class DOT     ( ) extends KeyWdToken { val text: String = "."       }
case class COLON   ( ) extends KeyWdToken { val text: String = ":"       }
case class LET     ( ) extends KeyWdToken { val text: String = "let"     }
case class NEW     ( ) extends KeyWdToken { val text: String = "new"     }
case class IF      ( ) extends KeyWdToken { val text: String = "if"      }
case class THEN    ( ) extends KeyWdToken { val text: String = "then"    }
case class ELSE    ( ) extends KeyWdToken { val text: String = "else"    }
case class ENDIF   ( ) extends KeyWdToken { val text: String = "endif"   }
case class SEND    ( ) extends KeyWdToken { val text: String = "send"    }
case class RECEIVE ( ) extends KeyWdToken { val text: String = "receive" }
case class SERVER  ( ) extends KeyWdToken { val text: String = "server"  }
case class BAR     ( ) extends KeyWdToken { val text: String = "|"       }
case class END     ( ) extends KeyWdToken { val text: String = "end"     }
case class LPAREN  ( ) extends KeyWdToken { val text: String = "("       }
case class RPAREN  ( ) extends KeyWdToken { val text: String = ")"       }
case class LCURLY  ( ) extends KeyWdToken { val text: String = "{"       }
case class RCURLY  ( ) extends KeyWdToken { val text: String = "}"       }
case class LSQUARE ( ) extends KeyWdToken { val text: String = "["       }
case class RSQUARE ( ) extends KeyWdToken { val text: String = "]"       }
case class COMMA   ( ) extends KeyWdToken { val text: String = ","       }
case class LARROW  ( ) extends KeyWdToken { val text: String = "<-"      }
case class RARROW  ( ) extends KeyWdToken { val text: String = "->"      }
case class TRUE    ( ) extends KeyWdToken { val text: String = "true"    }
case class FALSE   ( ) extends KeyWdToken { val text: String = "false"   }
case class PLUS    ( ) extends KeyWdToken { val text: String = "+"       }
case class DASH    ( ) extends KeyWdToken { val text: String = "-"       }
case class FSLASH  ( ) extends KeyWdToken { val text: String = "/"       }
case class PERC    ( ) extends KeyWdToken { val text: String = "%"       }
case class EQUAL   ( ) extends KeyWdToken { val text: String = "="       }
case class EQEQ    ( ) extends KeyWdToken { val text: String = "=="      }
case class NEQ     ( ) extends KeyWdToken { val text: String = "!="      }
case class LESS    ( ) extends KeyWdToken { val text: String = "<"       }
case class LESSEQ  ( ) extends KeyWdToken { val text: String = "<="      }
case class GRTR    ( ) extends KeyWdToken { val text: String = ">"       }
case class GRTREQ  ( ) extends KeyWdToken { val text: String = ">="      }
case class AND     ( ) extends KeyWdToken { val text: String = "&&"      }
case class OR      ( ) extends KeyWdToken { val text: String = "||"      }
case class CONS    ( ) extends KeyWdToken { val text: String = "::"      }
case class QMARK   ( ) extends KeyWdToken { val text: String = "?"       }
case class HEAD    ( ) extends KeyWdToken { val text: String = "*--"     }
case class TAIL    ( ) extends KeyWdToken { val text: String = "-**"     }

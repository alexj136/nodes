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
  ): Either [ LexerParserError , ( Map [ String , Name ] , NumName , T ) ] =
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

  def par: Parser [ Proc ] =
    LSQUARE() ~ rep1sep ( seq , BAR() ) ~ RSQUARE() ^^ {
      case l ~ p ~ r => putPos ( Proc fromList p , l , r )
    }

  def snd: Parser [ Proc ] = SEND() ~ exp ~ SEMI() ~ repsep ( ty , COMMA() ) ~
    SEMI () ~ repsep ( exp , COMMA() ) ~ DOT() ~ seq ^^ {
      case s ~ c ~ _ ~ ts ~ _ ~ ms ~ d ~ p =>
        putPos ( Send ( c , ts , ms , p ) , s , d )
    }

  def rcv: Parser [ Proc ] =
    RECEIVE() ~ exp ~ SEMI() ~ repsep ( name , COMMA() ) ~
    SEMI() ~ repsep ( nameTy , COMMA() ) ~ DOT() ~ seq ^^ {
      case r ~ c ~ _ ~ qs ~ _ ~ as ~ d ~ p =>
        putPos ( Receive ( false , c , qs , as , p ) , r , d )
    }

  def srv: Parser [ Proc ] =
    SERVER() ~ exp ~ SEMI() ~ repsep ( name , COMMA() ) ~
    SEMI() ~ repsep ( nameTy , COMMA() ) ~ DOT() ~ seq ^^ {
      case s ~ c ~ _ ~ qs ~ _ ~ as ~ d ~ p =>
        putPos ( Receive ( true , c , qs , as , p ) , s , d )
    }

  // Helper parser for server and receive to easily parse lists of
  // 'name: ty, ...' using repsep.
  def nameTy: Parser [ ( Name , SType ) ] = name ~ COLON() ~ ty ^^ {
    case n ~ _ ~ t => ( n , t )
  }

  def res: Parser [ Proc ] =
    NEW() ~ name ~ COLON() ~ ty ~ DOT() ~ seq ^^ {
      case nu ~ n ~ _ ~ t ~ d ~ p => putPos ( New ( n , t , p ) , nu , d )
    }

  def let: Parser [ Proc ] =
    LET() ~ name ~ COLON() ~ ty ~ EQUAL() ~ exp ~ DOT() ~ seq ^^ {
      case l ~ n ~ _ ~ t ~ _ ~ e ~ d ~ p =>
        putPos ( LetIn ( n , t , e , p ) , l , d )
    }

  def ite: Parser [ Proc ] =
    IF() ~ exp ~ THEN() ~ seq ~ ELSE() ~ seq ~ ENDIF() ^^ {
      case i ~ e ~ _ ~ p ~ _ ~ q ~ d =>
        putPos ( IfThenElse ( e , p , q ) , i , d )
    }

  def end: Parser [ Proc ] = END() ^^ { case end => putPos ( End , end , end ) }

  /**
   * Combinator parsers for types. No left-recursion here so it's nice and
   * straightforward.
   */

  def ty: Parser [ SType ] =
    tyInt | tyBool | tyChar | tyStr | tyList | tyChan | tyPair | tyVar

  def tyInt: Parser [ SType ] = TYINT() ^^ {
    case ti => putPos ( SInt , ti , ti )
  }

  def tyBool: Parser [ SType ] = TYBOOL() ^^ {
    case tb => putPos ( SBool , tb , tb )
  }

  def tyChar: Parser [ SType ] = TYCHAR() ^^ {
    case tc => putPos ( SKhar , tc , tc )
  }

  def tyStr: Parser [ SType ] = TYSTR() ^^ {
    case ts => putPos ( SList ( SKhar ) , ts , ts )
  }

  def tyList: Parser [ SType ] = LSQUARE() ~ ty ~ RSQUARE() ^^ {
    case l ~ t ~ r => putPos ( SList ( t ) , l , r )
  }

  def tyChan: Parser [ SType ] =
    AT() ~ LCURLY() ~ repsep ( name , COMMA() ) ~ SEMI() ~
    repsep ( ty , COMMA() ) ~ RCURLY() ^^ {
      case a ~ _ ~ qs ~ _ ~ ts ~ r =>
        putPos ( SChan ( qs map ( ( _ , List.empty ) ) , ts ) , a , r )
    }

  def tyPair: Parser [ SType ] = LPAREN() ~ ty ~ COMMA() ~ ty ~ RPAREN() ^^ {
    case l ~ t1 ~ _ ~ t2 ~ r => putPos ( SPair ( t1 , t2 ) , l , r )
  }

  def tyVar: Parser [ SType ] = name ^^ {
    case n => putPos ( SVar ( n , List.empty ) , n , n )
  }

  /**
   * Combinator parsers for expressions. The only left-recursive production in
   * the basic expression grammar for this language is the binary expression
   * production, so we add an extra expNoBinExp production to remove the left
   * recursion.
   */

  def exp: Parser [ Exp ] = binExp | expNoBinExp

  def expNoBinExp: Parser [ Exp ] = variable | intLiteral | trueLiteral |
    falseLiteral | kharLiteral | strLiteral | pair | unExp | parens |
    emptyList | list | stdOut | stdIn | stdErr

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

  def kharLiteral: Parser [ Exp ] = accept ( "POSTKHAR" , {
    case p @ POSTKHAR ( c ) => putPos ( KharLiteral ( c ) , p , p )
  } )

  def strLiteral: Parser [ Exp ] = accept ( "POSTSTR" , {
    case p @ POSTSTR ( s ) => putPos ( s , p , p )
  } )

  def trueLiteral: Parser [ Exp ] = TRUE() ^^ {
    case t => putPos ( BoolLiteral ( true ) , t , t )
  }

  def falseLiteral: Parser [ Exp ] = FALSE() ^^ {
    case f => putPos ( BoolLiteral ( false ) , f , f )
  }

  def pair: Parser [ Exp ] = LPAREN() ~ exp ~ COMMA() ~ exp ~ RPAREN() ^^ {
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

  def stdOut: Parser [ Exp ] = STDOUT() ^^ {
    case s => putPos ( ChanLiteral ( StdOutName ) , s , s )
  }

  def stdIn:  Parser [ Exp ] = STDIN() ^^ {
    case s => putPos ( ChanLiteral ( StdInName  ) , s , s )
  }

  def stdErr: Parser [ Exp ] = STDERR() ^^ {
    case s => putPos ( ChanLiteral ( StdErrName ) , s , s )
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
    , ( Map [ String , Name ] , NumName , List [ PostToken ] )
    ] = lex ( new PagedSeqReader ( PagedSeq.fromSource ( input ) ) ) match {
      case Success   ( tks , rest ) => Right ( PreToken.postLexAll ( tks ) )
      case NoSuccess ( msg , rest ) =>
        Left ( LexerError ( rest.pos.line , rest.pos.column , msg ) )
    }

  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f\n]+""".r

  def lex : Parser [ List [ PreToken ] ] = phrase ( rep1 (
    positioned { """[a-z_]+""".r         ^^ { PREIDENT ( _ )   } } |||
    positioned { """(0|[1-9]\d*)""".r    ^^ { PREINT   ( _ )   } } |||
    positioned { """\'(.|\n)\'""".r      ^^ { PREKHAR  ( _ )   } } |||
    positioned { """\"([^\"]|\n)*\"""".r ^^ { PRESTR   ( _ )   } } |||
    positioned { "!"                     ^^ { _ => BANG    ( ) } } |||
    positioned { "*"                     ^^ { _ => STAR    ( ) } } |||
    positioned { "."                     ^^ { _ => DOT     ( ) } } |||
    positioned { ":"                     ^^ { _ => COLON   ( ) } } |||
    positioned { ";"                     ^^ { _ => SEMI    ( ) } } |||
    positioned { "@"                     ^^ { _ => AT      ( ) } } |||
    positioned { "~"                     ^^ { _ => TILDE   ( ) } } |||
    positioned { "let"                   ^^ { _ => LET     ( ) } } |||
    positioned { "new"                   ^^ { _ => NEW     ( ) } } |||
    positioned { "if"                    ^^ { _ => IF      ( ) } } |||
    positioned { "then"                  ^^ { _ => THEN    ( ) } } |||
    positioned { "else"                  ^^ { _ => ELSE    ( ) } } |||
    positioned { "endif"                 ^^ { _ => ENDIF   ( ) } } |||
    positioned { "send"                  ^^ { _ => SEND    ( ) } } |||
    positioned { "receive"               ^^ { _ => RECEIVE ( ) } } |||
    positioned { "server"                ^^ { _ => SERVER  ( ) } } |||
    positioned { "of"                    ^^ { _ => OF      ( ) } } |||
    positioned { "|"                     ^^ { _ => BAR     ( ) } } |||
    positioned { "end"                   ^^ { _ => END     ( ) } } |||
    positioned { "("                     ^^ { _ => LPAREN  ( ) } } |||
    positioned { ")"                     ^^ { _ => RPAREN  ( ) } } |||
    positioned { "{"                     ^^ { _ => LCURLY  ( ) } } |||
    positioned { "}"                     ^^ { _ => RCURLY  ( ) } } |||
    positioned { "["                     ^^ { _ => LSQUARE ( ) } } |||
    positioned { "]"                     ^^ { _ => RSQUARE ( ) } } |||
    positioned { ","                     ^^ { _ => COMMA   ( ) } } |||
    positioned { "<-"                    ^^ { _ => LARROW  ( ) } } |||
    positioned { "->"                    ^^ { _ => RARROW  ( ) } } |||
    positioned { "true"                  ^^ { _ => TRUE    ( ) } } |||
    positioned { "false"                 ^^ { _ => FALSE   ( ) } } |||
    positioned { "+"                     ^^ { _ => PLUS    ( ) } } |||
    positioned { "-"                     ^^ { _ => DASH    ( ) } } |||
    positioned { "/"                     ^^ { _ => FSLASH  ( ) } } |||
    positioned { "%"                     ^^ { _ => PERC    ( ) } } |||
    positioned { "="                     ^^ { _ => EQUAL   ( ) } } |||
    positioned { "=="                    ^^ { _ => EQEQ    ( ) } } |||
    positioned { "!="                    ^^ { _ => NEQ     ( ) } } |||
    positioned { "<"                     ^^ { _ => LESS    ( ) } } |||
    positioned { "<="                    ^^ { _ => LESSEQ  ( ) } } |||
    positioned { ">"                     ^^ { _ => GRTR    ( ) } } |||
    positioned { ">="                    ^^ { _ => GRTREQ  ( ) } } |||
    positioned { "&&"                    ^^ { _ => AND     ( ) } } |||
    positioned { "||"                    ^^ { _ => OR      ( ) } } |||
    positioned { "::"                    ^^ { _ => CONS    ( ) } } |||
    positioned { "?"                     ^^ { _ => QMARK   ( ) } } |||
    positioned { "*--"                   ^^ { _ => HEAD    ( ) } } |||
    positioned { "-**"                   ^^ { _ => TAIL    ( ) } } |||
    positioned { "int"                   ^^ { _ => TYINT   ( ) } } |||
    positioned { "bool"                  ^^ { _ => TYBOOL  ( ) } } |||
    positioned { "char"                  ^^ { _ => TYCHAR  ( ) } } |||
    positioned { "string"                ^^ { _ => TYSTR   ( ) } } |||
    positioned { "stdout"                ^^ { _ => STDOUT  ( ) } } |||
    positioned { "stdin"                 ^^ { _ => STDIN   ( ) } } |||
    positioned { "stderr"                ^^ { _ => STDERR  ( ) } } ) )
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
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken )
}
object PreToken {
  def postLexAll (
    tokens: List [ PreToken ]
  ): ( Map [ String , Name ] , NumName , List [ PostToken ] ) = {
    var nameMap  : Map [ String , Name ] = Map.empty
    var nextName : NumName               = NumName ( 0 )
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
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken ) =
    nameMap get this.text match {
      case Some ( name ) => ( nameMap ,
        nextName      , POSTIDENT ( name     ).setPos ( this.pos ) )
      case None          => ( nameMap + ( ( this.text , nextName ) ) ,
        nextName.next , POSTIDENT ( nextName ).setPos ( this.pos ) )
    }
}
case class PREINT   ( text: String ) extends PreInfoToken ( text ) {
  /** To postlex an INT, we simply have to .toInt the string.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken ) =
    ( nameMap , nextName , POSTINT ( this.text.toInt ).setPos ( this.pos ) )
}

case class PREKHAR  ( text: String ) extends PreInfoToken ( text ) {
  /** To postlex an INT, we simply have to .toInt the string.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken ) =
    ( nameMap , nextName , POSTKHAR ( this.text.charAt ( 1 ) ).setPos ( this.pos ) )
}

case class PRESTR   ( text: String ) extends PreInfoToken ( text ) {
  /** To postlex an INT, we simply have to .toInt the string.
   */
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken ) =
    ( nameMap , nextName ,
      POSTSTR  ( stringToEscapedCharList ( this.text.init.tail ) )
      .setPos ( this.pos ) )

  def stringToEscapedCharList ( str: String ): Exp =
    ListExp ( escape ( str ).toList.map ( KharLiteral ( _ ) ) )

  /** Hacky way to deal with escape characters in strings. Scan through the
   *  string (implicitly converted to Seq[Char] and back) and look for the
   *  escape sequences. This implementation will not scale to deal with \" or \'
   *  and therefore something more sophisticated will eventually be required in
   *  the lex method to handle these.
   */
  def escape ( s: Seq[Char] ): Seq[Char] = s match {
    case '\\' +: 'n'  +: t => '\n' +: escape ( t )
    case '\\' +: 't'  +: t => '\t' +: escape ( t )
    case '\\' +: '\\' +: t => '\\' +: escape ( t )
    case          c   +: t =>   c  +: escape ( t )
    case Nil               => Nil
  }
}

sealed trait PostToken extends Token
sealed trait PostInfoToken extends PostToken
case class POSTIDENT ( name: Name ) extends PostInfoToken
case class POSTINT   ( num:  Int  ) extends PostInfoToken
case class POSTKHAR  ( khar: Char ) extends PostInfoToken
case class POSTSTR   ( str:  Exp  ) extends PostInfoToken

sealed abstract class KeyWdToken extends PreToken with PostToken {
  val text: String
  override def postLex (
    nameMap: Map [ String , Name ] ,
    nextName: NumName
  ): ( Map [ String , Name ] , NumName , PostToken ) =
    ( nameMap , nextName , this )
}

case class BANG    ( ) extends KeyWdToken { val text: String = "!"       }
case class STAR    ( ) extends KeyWdToken { val text: String = "*"       }
case class DOT     ( ) extends KeyWdToken { val text: String = "."       }
case class COLON   ( ) extends KeyWdToken { val text: String = ":"       }
case class SEMI    ( ) extends KeyWdToken { val text: String = ";"       }
case class AT      ( ) extends KeyWdToken { val text: String = "@"       }
case class TILDE   ( ) extends KeyWdToken { val text: String = "~"       }
case class LET     ( ) extends KeyWdToken { val text: String = "let"     }
case class NEW     ( ) extends KeyWdToken { val text: String = "new"     }
case class IF      ( ) extends KeyWdToken { val text: String = "if"      }
case class THEN    ( ) extends KeyWdToken { val text: String = "then"    }
case class ELSE    ( ) extends KeyWdToken { val text: String = "else"    }
case class ENDIF   ( ) extends KeyWdToken { val text: String = "endif"   }
case class SEND    ( ) extends KeyWdToken { val text: String = "send"    }
case class RECEIVE ( ) extends KeyWdToken { val text: String = "receive" }
case class SERVER  ( ) extends KeyWdToken { val text: String = "server"  }
case class OF      ( ) extends KeyWdToken { val text: String = "of"      }
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
case class TYINT   ( ) extends KeyWdToken { val text: String = "int"     }
case class TYBOOL  ( ) extends KeyWdToken { val text: String = "bool"    }
case class TYCHAR  ( ) extends KeyWdToken { val text: String = "char"    }
case class TYSTR   ( ) extends KeyWdToken { val text: String = "string"  }
case class STDOUT  ( ) extends KeyWdToken { val text: String = "stdout"  }
case class STDIN   ( ) extends KeyWdToken { val text: String = "stdin"   }
case class STDERR  ( ) extends KeyWdToken { val text: String = "stderr"  }

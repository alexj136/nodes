package newparser

/** Implements a parser for the nodes language using scala's parser combinators.
 *  Parsing is subdivided into separate lexing, processing and parsing phases.
 *  - Lexing converts a String of nodes code into a List [ PreToken ]. PreTokens
 *  are tokens where identifiers, ints and such carry the text that was lexed
 *  to produce them.
 *  - The processing step converts PreTokens into PostTokens, changing the
 *  representation of names from Strings to Integers, providing a map between
 *  string and integer names for printing purposes.
 *  - Parsing converts a List [ PostToken ] into a syntax.Proc.
 */

import syntax._
import scala.util.parsing.input._
import scala.util.parsing.combinator._

/** Responsible for converting a List [ PostToken ] into a syntax.Proc via the
 *  ... method.
 */

sealed abstract class LexerParserError ( msg: String )
case class LexerError  ( msg: String ) extends LexerParserError ( msg )
case class ParserError ( msg: String ) extends LexerParserError ( msg )

object LexAndParse extends Function1
  [ String
  , Either [ LexerParserError , ( Map [ String , Name ] , Name , Proc ) ] ] {

  def apply (
    input: String
  ): Either [ LexerParserError , ( Map [ String , Name ] , Name , Proc ) ] =
    for {
      lexed  <- Lexer  ( input    ).right
      parsed <- Parser ( lexed._3 ).right
    } yield ( lexed._1 , lexed._2 , parsed )
}

object Parser extends Parsers {

  def apply ( input: List [ PostToken ] ): Either [ ParserError , Proc ] =
    phrase ( proc ) ( new TokenReader ( input ) ) match {
      case Success   ( prc , rest ) => Right ( prc                 )
      case NoSuccess ( msg , rest ) => Left  ( ParserError ( msg ) )
    }

  override type Elem = PostToken

  def name: Parser [ Name ] =
    accept ( "POSTIDENT" , { case POSTIDENT ( n ) => n } )

  def proc: Parser [ Proc ] = par ^^ { Proc fromList _ }

  def par: Parser [ List [ Proc ] ] = rep1sep (
    end | snd | rcv | srv | res | let | ite | LPAREN ~> proc <~ RPAREN , BAR )

  def snd: Parser [ Proc ] =
    SEND ~ exp ~ COLON ~ exp ~ DOT ~ proc ^^ {
      case _ ~ c ~ _ ~ m ~ _ ~ p => Send ( c , m , p )
    }

  def rcv: Parser [ Proc ] =
    RECEIVE ~ exp ~ COLON ~ name ~ DOT ~ proc ^^ {
      case _ ~ c ~ _ ~ b ~ _ ~ p => Receive ( false , c , b , p )
    }

  def srv: Parser [ Proc ] =
    SERVER ~ exp ~ COLON ~ name ~ DOT ~ proc ^^ {
      case _ ~ c ~ _ ~ b ~ _ ~ p => Receive ( true , c , b , p )
    }

  def res: Parser [ Proc ] =
    NEW ~ name ~ DOT ~ proc ^^ {
      case _ ~ n ~ _ ~ p => New ( n , p )
    }

  def let: Parser [ Proc ] =
    LET ~ name ~ EQUAL ~ exp ~ DOT ~ proc ^^ {
      case _ ~ n ~ _ ~ e ~ _ ~ p => LetIn ( n , e , p )
    }

  def ite: Parser [ Proc ] =
    IF ~ exp ~ THEN ~ proc ~ ELSE ~ proc ~ ENDIF ^^ {
      case _ ~ e ~ _ ~ p ~ _ ~ q ~ _ => IfThenElse ( e , p , q )
    }

  def end: Parser [ Proc ] =
    END ^^ {
      _ => End
    }

  /**
   * Combinator parsers for expressions. The only left-recursive production in
   * the basic expression grammar for this language is the binary expression
   * production, so we add an extra expNoBinExp production to remove the left
   * recursion.
   */

  def exp: Parser [ Exp ] = binExp | expNoBinExp

  def expNoBinExp: Parser [ Exp ] = variable | intLiteral | trueLiteral |
    falseLiteral | chanLiteral | pair | unExp | LPAREN ~> exp <~ RPAREN

  def binExp: Parser [ Exp ] = expNoBinExp ~ binOpTy ~ exp ^^ {
    case l ~ op ~ r => BinExp ( op , l , r )
  }

  def variable: Parser [ Exp ] = name ^^ { Variable ( _ ) }

  def intLiteral: Parser [ Exp ] =
    accept ( "POSTINT" , { case POSTINT ( i ) => IntLiteral ( i ) } )

  def trueLiteral: Parser [ Exp ] = TRUE ^^ { _ => BoolLiteral ( true ) }

  def falseLiteral: Parser [ Exp ] = FALSE ^^ { _ => BoolLiteral ( false ) }

  def chanLiteral: Parser [ Exp ] = accept ( "POSTCHAN" , {
    case POSTCHAN ( c ) => ChanLiteral ( c )
  } )

  def pair: Parser [ Exp ] = LCURLY ~ exp ~ COMMA ~ exp ~ RCURLY ^^ {
    case _ ~ l ~ _ ~ r ~ _ => Pair ( l , r )
  }

  def unExp: Parser [ Exp ] = unOpTy ~ exp ^^ {
    case op ~ e => UnExp ( op , e )
  }

  def uMinus: Parser [ Exp ] = DASH ~ exp ^^ {
    case _ ~ e => BinExp ( Sub , IntLiteral ( 0 ) , e )
  }

  def binOpTy: Parser [ BinOp ] =
    PLUS   ^^ { _ => Add       } |
    DASH   ^^ { _ => Sub       } |
    STAR   ^^ { _ => Mul       } |
    FSLASH ^^ { _ => Div       } |
    PERC   ^^ { _ => Mod       } |
    EQEQ   ^^ { _ => Equal     } |
    NEQ    ^^ { _ => NotEqual  } |
    LESS   ^^ { _ => Less      } |
    LESSEQ ^^ { _ => LessEq    } |
    GRTR   ^^ { _ => Greater   } |
    GRTREQ ^^ { _ => GreaterEq } |
    AND    ^^ { _ => And       } |
    OR     ^^ { _ => Or        }

  def unOpTy: Parser [ UnOp ] =
    BANG   ^^ { _ => Not    } |
    LARROW ^^ { _ => PLeft  } |
    RARROW ^^ { _ => PRight }
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

  def apply ( input: String ): Either
    [ LexerError
    , ( Map [ String , Name ] , Name , List [ PostToken ] )
    ] = lex ( new CharSequenceReader ( input ) ) match {
      case Success   ( tks , rest ) => Right ( PreToken.processAll ( tks ) )
      case NoSuccess ( msg , rest ) => Left  ( LexerError          ( msg ) )
    }

  override def skipWhitespace = true
  override val whiteSpace = """[ \t\r\f\n]+""".r

  def lex : Parser [ List [ PreToken ] ] = phrase ( rep1 (
    """[a-z_]+""".r      ^^ { PREIDENT ( _ )  } |||
    """\$[a-z_]+""".r    ^^ { PRECHAN  ( _ )  } |||
    """(0|[1-9]\d*)""".r ^^ { PREINT   ( _ )  } |||
    "!"                  ^^ { _ => BANG       } |||
    "*"                  ^^ { _ => STAR       } |||
    "."                  ^^ { _ => DOT        } |||
    ":"                  ^^ { _ => COLON      } |||
    "let"                ^^ { _ => LET        } |||
    "new"                ^^ { _ => NEW        } |||
    "if"                 ^^ { _ => IF         } |||
    "then"               ^^ { _ => THEN       } |||
    "else"               ^^ { _ => ELSE       } |||
    "endif"              ^^ { _ => ENDIF      } |||
    "send"               ^^ { _ => SEND       } |||
    "receive"            ^^ { _ => RECEIVE    } |||
    "server"             ^^ { _ => SERVER     } |||
    "|"                  ^^ { _ => BAR        } |||
    "end"                ^^ { _ => END        } |||
    "("                  ^^ { _ => LPAREN     } |||
    ")"                  ^^ { _ => RPAREN     } |||
    "{"                  ^^ { _ => LCURLY     } |||
    "}"                  ^^ { _ => RCURLY     } |||
    ","                  ^^ { _ => COMMA      } |||
    "<-"                 ^^ { _ => LARROW     } |||
    "->"                 ^^ { _ => RARROW     } |||
    "true"               ^^ { _ => TRUE       } |||
    "false"              ^^ { _ => FALSE      } |||
    "+"                  ^^ { _ => PLUS       } |||
    "-"                  ^^ { _ => DASH       } |||
    "/"                  ^^ { _ => FSLASH     } |||
    "%"                  ^^ { _ => PERC       } |||
    "="                  ^^ { _ => EQUAL      } |||
    "=="                 ^^ { _ => EQEQ       } |||
    "!="                 ^^ { _ => NEQ        } |||
    "<"                  ^^ { _ => LESS       } |||
    "<="                 ^^ { _ => LESSEQ     } |||
    ">"                  ^^ { _ => GRTR       } |||
    ">="                 ^^ { _ => GRTREQ     } |||
    "&&"                 ^^ { _ => AND        } |||
    "||"                 ^^ { _ => OR         } ) )
}

/** Below we have the token classes. We have PreTokens and PostTokens to
 *  represent tokens before and after the processing of lexed information.
 *  KeyWdTokens are both Pre and Post tokens because they require no processing.
 *  Tokens that contain information are divided into separate Pre and Post
 *  InfoToken classes as they require processing.
 */

sealed abstract class PreToken {
  def process (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken )
}
object PreToken {
  def processAll (
    tokens: List [ PreToken ]
  ): ( Map [ String , Name ] , Name , List [ PostToken ] ) = {
    var nameMap  : Map [ String , Name ] = Map.empty
    var nextName : Name                  = Name ( 0 )
    var done     : List [ PostToken ]    = List ( )
    var todo     : List [ PreToken  ]    = tokens
    while ( ! todo.isEmpty ) {
      val todoHeadProcessed = todo.head.process ( nameMap , nextName )
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
  override def process (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    nameMap get this.text match {
      case Some ( name ) => ( nameMap , nextName , POSTIDENT ( name ) )
      case None          => ( nameMap + ( ( this.text , nextName ) ) ,
        nextName.next , POSTIDENT ( nextName ) )
    }
}
case class PRECHAN  ( text: String ) extends PreInfoToken ( text ) {
  /** Processing a CHAN is the same as with IDENTs.
   */
  override def process (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    nameMap get this.text match {
      case Some ( name ) => ( nameMap , nextName , POSTCHAN ( name ) )
      case None          => ( nameMap + ( ( this.text , nextName ) ) ,
        nextName.next , POSTCHAN ( nextName ) )
    }
}
case class PREINT   ( text: String ) extends PreInfoToken ( text ) {
  /** To process an INT, we simply have to .toInt the string.
   */
  override def process (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    ( nameMap , nextName , POSTINT ( this.text.toInt ) )
}

sealed trait PostToken
sealed trait PostInfoToken extends PostToken

case class POSTIDENT ( name: Name ) extends PostInfoToken
case class POSTCHAN  ( name: Name ) extends PostInfoToken
case class POSTINT   ( num:  Int  ) extends PostInfoToken

sealed abstract class KeyWdToken extends PreToken with PostToken {
  val text: String
  override def process (
    nameMap: Map [ String , Name ] ,
    nextName: Name
  ): ( Map [ String , Name ] , Name , PostToken ) =
    ( nameMap , nextName , this )
}

case object BANG    extends KeyWdToken { val text: String = "!"       }
case object STAR    extends KeyWdToken { val text: String = "*"       }
case object DOT     extends KeyWdToken { val text: String = "."       }
case object COLON   extends KeyWdToken { val text: String = ":"       }
case object LET     extends KeyWdToken { val text: String = "let"     }
case object NEW     extends KeyWdToken { val text: String = "new"     }
case object IF      extends KeyWdToken { val text: String = "if"      }
case object THEN    extends KeyWdToken { val text: String = "then"    }
case object ELSE    extends KeyWdToken { val text: String = "else"    }
case object ENDIF   extends KeyWdToken { val text: String = "endif"   }
case object SEND    extends KeyWdToken { val text: String = "send"    }
case object RECEIVE extends KeyWdToken { val text: String = "receive" }
case object SERVER  extends KeyWdToken { val text: String = "server"  }
case object BAR     extends KeyWdToken { val text: String = "|"       }
case object END     extends KeyWdToken { val text: String = "end"     }
case object LPAREN  extends KeyWdToken { val text: String = "("       }
case object RPAREN  extends KeyWdToken { val text: String = ")"       }
case object LCURLY  extends KeyWdToken { val text: String = "{"       }
case object RCURLY  extends KeyWdToken { val text: String = "}"       }
case object COMMA   extends KeyWdToken { val text: String = ","       }
case object LARROW  extends KeyWdToken { val text: String = "<-"      }
case object RARROW  extends KeyWdToken { val text: String = "->"      }
case object TRUE    extends KeyWdToken { val text: String = "true"    }
case object FALSE   extends KeyWdToken { val text: String = "false"   }
case object PLUS    extends KeyWdToken { val text: String = "+"       }
case object DASH    extends KeyWdToken { val text: String = "-"       }
case object FSLASH  extends KeyWdToken { val text: String = "/"       }
case object PERC    extends KeyWdToken { val text: String = "%"       }
case object EQUAL   extends KeyWdToken { val text: String = "="       }
case object EQEQ    extends KeyWdToken { val text: String = "=="      }
case object NEQ     extends KeyWdToken { val text: String = "!="      }
case object LESS    extends KeyWdToken { val text: String = "<"       }
case object LESSEQ  extends KeyWdToken { val text: String = "<="      }
case object GRTR    extends KeyWdToken { val text: String = ">"       }
case object GRTREQ  extends KeyWdToken { val text: String = ">="      }
case object AND     extends KeyWdToken { val text: String = "&&"      }
case object OR      extends KeyWdToken { val text: String = "||"      }

package test

import syntax._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.BooleanOperators

object ArbitraryTypes {

  val genSend: Gen[Proc] = for {
    ch   <- genExp
    msg  <- genExp
    next <- genProc
  } yield Send(ch, msg, next)

  val genReceive: Gen[Proc] = for {
    srv  <- arbitrary[Boolean]
    ch   <- genExp
    bind <- genName
    next <- genProc
  } yield Receive(srv, ch, bind, next)

  val genNew: Gen[Proc] = for {
    bind <- genName
    next <- genProc
  } yield New(bind, next)

  val genParallel: Gen[Proc] = for {
    l <- genProc
    r <- genProc
  } yield Parallel(l, r)

  val genLetIn: Gen[Proc] = for {
    bind <- genName
    exp  <- genExp
    next <- genProc
  } yield LetIn(bind, exp, next)

  val genIfThenElse: Gen[Proc] = for {
    cond <- genExp
    t    <- genProc
    f    <- genProc
  } yield IfThenElse(cond, t, f)

  val genEnd: Gen[Proc] = const(End)

  def genProc: Gen[Proc] = lzy(frequency(
    (  2 , genSend       ) ,
    (  2 , genReceive    ) ,
    (  1 , genNew        ) ,
    (  1 , genParallel   ) ,
    (  1 , genLetIn      ) ,
    (  1 , genIfThenElse ) ,
    ( 20 , genEnd        ) )
  )

  val genVariable: Gen[Exp] = for {
    id <- genName
  } yield Variable(id)

  val genIntLiteral: Gen[Exp] = for {
    n <- arbitrary[Int]
  } yield IntLiteral(n)

  val genBoolLiteral: Gen[Exp] = for {
    b <- arbitrary[Boolean]
  } yield BoolLiteral(b)

  val genChanLiteral: Gen[Exp] = for {
    c <- genName
  } yield ChanLiteral(c)

  val genPair: Gen[Exp] = for {
    l <- genExp
    r <- genExp
  } yield Pair(l, r)

  val genUnExp: Gen[Exp] = for {
    op  <- oneOf(
      const( Not    ),
      const( PLeft  ),
      const( PRight ))
    exp <- genExp
  } yield UnExp(op, exp)

  val genBinExp: Gen[Exp] = for {
    op <- oneOf(
      const ( Add       ),
      const ( Sub       ),
      const ( Mul       ),
      const ( Div       ),
      const ( Mod       ),
      const ( Equal     ),
      const ( NotEqual  ),
      const ( Less      ),
      const ( LessEq    ),
      const ( Greater   ),
      const ( GreaterEq ),
      const ( And       ),
      const ( Or        ))
    l  <- genExp
    r  <- genExp
  } yield BinExp(op, l, r)

  def genExp: Gen[Exp] = lzy(frequency(
    ( 10 , genVariable    ) ,
    ( 10 , genIntLiteral  ) ,
    ( 10 , genBoolLiteral ) ,
    ( 10 , genChanLiteral ) ,
    (  3 , genPair        ) ,
    (  6 , genUnExp       ) ,
    (  3 , genBinExp      ) )
  )

  def genName: Gen[Name] = for {
    id <- lzy(arbitrary[Int])
  } yield Name(id)

  implicit val arbitraryProc: Arbitrary[Proc] = Arbitrary(genProc)
  implicit val arbitraryExp : Arbitrary[Exp ] = Arbitrary(genExp )
  implicit val arbitraryName: Arbitrary[Name] = Arbitrary(genName)
}

object ProcProperties extends Properties("Proc") {
  import ArbitraryTypes._

  property("alphaEquivIsReflexive") = Prop.forAll { ( p: Proc ) => {
    (p alphaEquiv p).nonEmpty
  }}

  property("alphaEquivSimple") = Prop.forAll {
    ( n0: Name, n1: Name, n2: Name ) => { (n0 != n1 && n0 != n2) ==>
      (Send(ChanLiteral(n0), Variable(n1), End).alphaEquiv(
        Send(ChanLiteral(n1), Variable(n2), End))).nonEmpty
    }
  }
}

object ParserProperties extends Properties("Parser") {
  import parser._
  import scala.io.Source
  import scala.util.parsing.input.CharSequenceReader

  /* Lexer tests */

  def lexesAs ( input: String , expectedOutput: List [ PreToken ] ): Boolean =
    Lexer.lex ( new CharSequenceReader ( input ) ) match {
      case Lexer.Success ( tks , _ ) => tks == expectedOutput
      case _ => false
    }

  def lexerFails ( input: String ): Boolean =
    Lexer.lex ( new CharSequenceReader ( input ) ) match {
      case Lexer.Success ( _ , _ ) => false
      case _ => true
    }

  property("keywordNotIdent") = lexesAs ( "send" , List ( SEND ) )

  property("identNotTwoKeywords") =
    lexesAs ( "sendsend" , List ( PREIDENT ( "sendsend" ) ) )

  property("twoKeywordsNotIdent") =
    lexesAs ( "send send" , List ( SEND , SEND ) ) &&
    lexesAs ( "receive send" , List ( RECEIVE , SEND ) ) &&
    lexesAs ( "receive end" , List ( RECEIVE , END ) )

  property("twoIdents") =
    lexesAs ( "a b" , List ( PREIDENT ( "a" ) , PREIDENT ( "b" ) ) )

  property("justChan") = lexesAs ( "$send" , List ( PRECHAN ( "$send" ) ) )

  property("justInt") = lexesAs ( "25" , List ( PREINT ( "25" ) ) )

  /* Parser tests */

  def parsesAs [ T ] (
    production: Parser.Parser [ T ] ,
    input: String ,
    expectedOutput: T
  ): Boolean =
    lexAndParse ( production , Source fromString input ).right
      .map ( _._3 == expectedOutput ).right.getOrElse ( false )

  property("Procs") = {
    parsesAs ( Parser.proc , "end" , End ) &&
    parsesAs ( Parser.proc , "[ end | end | end ]" ,
      Parallel ( End , Parallel ( End , End ) ) ) &&
    parsesAs ( Parser.proc , "send 1 : 2 . end" ,
      Send ( IntLiteral ( 1 ) , IntLiteral ( 2 ) , End ) ) &&
    parsesAs ( Parser.proc , " [ send 1 : 2 . end | receive 1 : a . end ] " ,
      Parallel ( Send ( IntLiteral ( 1 ) , IntLiteral ( 2 ) , End ) ,
        Receive ( false , IntLiteral ( 1 ) , Name ( 0 ) , End ) ) )
  }

  property("Exps") = {
    parsesAs ( Parser.exp , "a" , Variable ( Name ( 0 ) ) ) &&
    parsesAs ( Parser.exp , "a + b" ,
      BinExp ( Add , Variable ( Name ( 0 ) ) , Variable ( Name ( 1 ) ) ) ) &&
    parsesAs ( Parser.exp , "a + b + c" ,
      BinExp ( Add , Variable ( Name ( 0 ) ) ,
        BinExp ( Add , Variable ( Name ( 1 ) ) ,
          Variable ( Name ( 2 ) ) ) ) ) &&
    ( lexAndParse ( Parser.exp , Source fromString "a + b + c" ) ==
      lexAndParse ( Parser.exp , Source fromString "a + ( b + c )" ) )
  }
}

object TurnerMachineProperties extends Properties("TurnerMachineState") {
  import parser._
  import interpreter.turner.runWithTurnerMachine
  import scala.io.Source

  val names: Map[Name, String] = (((0 to 51) map (n => Name(n)))
    .zip(((('a' to 'z') ++ ('A' to 'Z')) map (s => s.toString)))).toMap
    
  val next: Name = Name(52)

  property("simpleProcess") = {
    val procStr: String = " [ receive $a : y . send y : y . end | " +
      "send $a : $x . end ] "
    lexAndParse ( Parser.proc , Source fromString procStr ) match {
      case Right ( ( nmap , nn , proc ) ) => {
        runWithTurnerMachine ( proc , nmap.map ( _.swap ) , nn )._1.listify
          .filter( _ != End )
          .==( List ( Send ( ChanLiteral ( nmap ( "$x" ) ) ,
            ChanLiteral ( nmap ( "$x" ) ) , End ) ) )
      }
      case Left ( _ ) => false
    }
  }

  property("addThreeNumbers") = {

    /* proc =
     *  [
     *    send a : x . end |
     *    send a : y . end |
     *    send a : z . end |
     *    receive a : b . receive a : c . receive a : d . [
     *      send a : b + c + d . end |
     *      receive a : e . send a : e . end
     *    ]
     *  ]
     *
     * should evaluate to =
     *
     *  send a : (x+y+z) . end
     */
    val proc: Function3[Int, Int, Int, Proc] = ( x: Int, y: Int, z: Int ) =>
      Proc.fromList(List(
        Send(ChanLiteral(Name(0)), IntLiteral(x), End),
        Send(ChanLiteral(Name(0)), IntLiteral(y), End),
        Send(ChanLiteral(Name(0)), IntLiteral(z), End),
        Receive(false, ChanLiteral(Name(0)), Name(1),
          Receive(false, ChanLiteral(Name(0)), Name(2),
          Receive(false, ChanLiteral(Name(0)), Name(3),
          Send(ChanLiteral(Name(4)), BinExp(Add, BinExp(Add, Variable(Name(1)),
            Variable(Name(2))), Variable(Name(3))), End)))),
        Receive(false, ChanLiteral(Name(4)), Name(5),
          Send(ChanLiteral(Name(0)), Variable(Name(5)), End))))
    Prop.forAll { ( x: Int, y: Int, z: Int ) => {
      val procPost: Proc = runWithTurnerMachine(proc(x, y, z), names, next)._1
      Proc.fromList(procPost.listify.filter( _ != End )).alphaEquiv(
        Proc.fromList(Send(ChanLiteral(Name(0)), IntLiteral(x + y + z), End)
          .listify.filter( _ != End ))).nonEmpty
    }}
  }
}

object TypecheckProperties extends Properties("Typecheck") {
  import parser._
  import typecheck._
  import typecheck.Typecheck._
  import ArbitraryTypes._
  import scala.io.Source

  property("dequantifyPLeft") = Prop.forAll { ( n: Int ) => { ( n > 1 ) ==> {
    dequantify(typeOfUnOp(PLeft), new Name(n)) ==
      (SFunc(SPair(SVar(new Name(n)), SVar(new Name(n + 1))), SVar(new Name(n)))
      , new Name(n + 2))
  }}}

  property("dequantifyPRight") = Prop.forAll { ( n: Int ) => { ( n > 1 ) ==> {
    dequantify(typeOfUnOp(PRight), new Name(n)) ==
      (SFunc(SPair(SVar(new Name(n)), SVar(new Name(n + 1)))
      , SVar(new Name(n + 1))), new Name(n + 2))
  }}}

  property("unifyBadlyTypedExpFails") = {
    val exp: Exp = UnExp(PLeft, IntLiteral(1))
    val (_, constraints: ConstraintSet, _) =
      constraintsExp(exp, Map.empty, new Name(0))
    unify(constraints) == None
  }

  property("unifyArbitraryExpNoCrash") = Prop.forAll { exp: Exp => {
    // Free variables are SInts in this typing environment
    val (_, constraints: ConstraintSet, _) = constraintsExp(exp,
      ((exp.free union exp.chanLiterals) map ( n => ( n , SInt ) ) ).toMap,
      new Name(0))
    unify ( constraints )
    true
  }}

  def checks(procStr: String): Boolean = checkProc ( lexAndParse (
    Parser.proc , Source fromString procStr ).right.get._3 ).isDefined

  property("reallyReallySimpleProcsTypeCheck") =
    checks ( " end " ) &&
    checks ( " [ end ] " ) &&
    checks ( " [ end | end | end ] " )

  property("reallySimpleProcsTypeCheck") =
    checks ( " receive $a : y . end " ) &&
    checks ( " send $a : 12 . end " ) &&
    checks ( " new a . end " ) &&
    checks ( " let x = -> { 10 , 11 } . end " ) &&
    checks ( " server $a : y . end " )

  property("simpleProcTypeChecks") =
    checks ( " [ receive $a : y . send y : y . end | send $a : $x . end ] " )
}

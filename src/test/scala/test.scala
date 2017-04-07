package test

import syntax._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalacheck.Prop.BooleanOperators

object ArbitraryTypes {

  def smallListOf[A](gen: Gen[A]): Gen[List[A]] = for {
    g <- listOfN(Math.round(scala.util.Random.nextGaussian * 5).toInt.abs, gen)
  } yield g

  val genSend: Gen[Proc] = for {
    ch   <- genExp
    ts   <- smallListOf(genSType)
    ms   <- smallListOf(genExp)
    next <- genProc
  } yield Send(ch, ts, ms, next)

  val genReceive: Gen[Proc] = for {
    srv  <- arbitrary[Boolean]
    ch   <- genExp
    qs   <- smallListOf(genName)
    bs   <- smallListOf(genName)
    ts   <- listOfN(bs.size, genSType)
    next <- genProc
  } yield Receive(srv, ch, qs, bs zip ts, next)

  val genNew: Gen[Proc] = for {
    bind <- genName
    ty   <- genSType
    next <- genProc
  } yield New(bind, ty, next)

  val genParallel: Gen[Proc] = for {
    l <- genProc
    r <- genProc
  } yield Parallel(l, r)

  val genLetIn: Gen[Proc] = for {
    bind <- genName
    ty   <- genSType
    exp  <- genExp
    next <- genProc
  } yield LetIn(bind, ty, exp, next)

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
      const( PRight ),
      const( Empty  ),
      const( Head   ),
      const( Tail   ))
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
      const ( Or        ),
      const ( Cons      ))
    l  <- genExp
    r  <- genExp
  } yield BinExp(op, l, r)

  val genListExp: Gen[Exp] = for {
    es <- smallListOf(genExp)
  } yield ListExp(es)

  def genExp: Gen[Exp] = lzy(frequency(
    ( 10 , genVariable    ) ,
    ( 10 , genIntLiteral  ) ,
    ( 10 , genBoolLiteral ) ,
//  ( 10 , genChanLiteral ) , not allowed in source - only for interpreters
    (  3 , genPair        ) ,
    (  6 , genUnExp       ) ,
    (  3 , genBinExp      ) ,
    (  3 , genListExp     ) )
  )

  val genSProc: Gen[SType] = const(SProc)
  val genSInt:  Gen[SType] = const(SInt)
  val genSBool: Gen[SType] = const(SBool)
  val genSKhar: Gen[SType] = const(SKhar)

  val genSChan: Gen[SType] = for {
    qs <- smallListOf(genName)
    ts <- smallListOf(genSType)
  } yield SChan(qs, ts)

  val genSList: Gen[SType] = for {
    t <- genSType
  } yield SList(t)

  val genSPair: Gen[SType] = for {
    l <- genSType
    r <- genSType
  } yield SPair(l, r)

  val genSVar: Gen[SType] = for {
    x <- genName
  } yield SVar(x)

  val genSFunc: Gen[SType] = for {
    a <- genSType
    r <- genSType
  } yield SFunc(a, r)

  def genSType: Gen[SType] = lzy(frequency(
    ( 10 , genSProc       ) ,
    ( 10 , genSInt        ) ,
    ( 10 , genSBool       ) ,
    ( 10 , genSKhar       ) ,
    (  6 , genSChan       ) ,
    (  6 , genSList       ) ,
    (  3 , genSPair       ) ,
    ( 10 , genSVar        ) ,
    (  3 , genSFunc       ) )
  )

  def genName: Gen[Name] = for {
    id <- lzy(arbitrary[Int])
  } yield NumName(id)

  implicit val arbitraryProc : Arbitrary[Proc ] = Arbitrary(genProc )
  implicit val arbitraryExp  : Arbitrary[Exp  ] = Arbitrary(genExp  )
  implicit val arbitrarySType: Arbitrary[SType] = Arbitrary(genSType)
  implicit val arbitraryName : Arbitrary[Name ] = Arbitrary(genName )
}

object ProcProperties extends Properties("Proc") {
  import ArbitraryTypes._

  property("alphaEquivIsReflexive") = Prop.forAll { ( p: Proc ) => {
    (p alphaEquiv p).nonEmpty
  }}

  property("alphaEquivSimple") = Prop.forAll {
    ( n0: Name, n1: Name, n2: Name ) => { (n0 != n1 && n0 != n2) ==>
      (Send(ChanLiteral(n0), List(), List(Variable(n1)), End).alphaEquiv(
        Send(ChanLiteral(n1), List(), List(Variable(n2)), End))).nonEmpty
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

  property("keywordNotIdent") = lexesAs ( "send" , List ( SEND() ) )

  property("consNotTwoColons") = lexesAs ( "::" , List ( CONS() ) )

  property("twoColonsNotCons") = lexesAs ( ": :" , List ( COLON() , COLON() ) )

  property("identNotTwoKeywords") =
    lexesAs ( "sendsend" , List ( PREIDENT ( "sendsend" ) ) )

  property("twoKeywordsNotIdent") =
    lexesAs ( "send send" , List ( SEND() , SEND() ) ) &&
    lexesAs ( "receive send" , List ( RECEIVE() , SEND() ) ) &&
    lexesAs ( "receive end" , List ( RECEIVE() , END() ) )

  property("twoIdents") =
    lexesAs ( "a b" , List ( PREIDENT ( "a" ) , PREIDENT ( "b" ) ) )

  property("justInt") = lexesAs ( "25" , List ( PREINT ( "25" ) ) )

  /* Parser tests */

  def parsesAs [ T ] (
    production: Parser.Parser [ T ] ,
    input: String ,
    expectedOutput: T
  ): Boolean =
    lexAndParse ( production , Source fromString input ).right
      .map ( _._3 == expectedOutput ).right.getOrElse ( false )

  property("charPreserved") = Prop.forAll { ( c: Char ) =>
    parsesAs ( Parser.exp , s"'$c'" ,  KharLiteral ( c ) )
  }

  property("Procs") = {
    parsesAs ( Parser.proc , "end" , End ) &&
    parsesAs ( Parser.proc , "[ end | end | end ]" ,
      Parallel ( End , Parallel ( End , End ) ) ) &&
    parsesAs ( Parser.proc , "send 1 ; ; 2 . end" ,
      Send ( IntLiteral ( 1 ) , List ( ) ,
        List ( IntLiteral ( 2 ) ) , End ) ) &&
    parsesAs ( Parser.proc ,
      " [ send 1 ; ; 2 . end | receive 1 ; ; a : int . end ] " ,
      Parallel ( Send ( IntLiteral ( 1 ) , List ( ) ,
        List ( IntLiteral ( 2 ) ) , End ) ,
        Receive ( false , IntLiteral ( 1 ) , List ( ) ,
          List ( ( NumName ( 0 ) , SInt ) ) , End ) ) )
  }

  property("Exps") = {
    parsesAs ( Parser.exp , "a" , Variable ( NumName ( 0 ) ) ) &&
    parsesAs ( Parser.exp , "a + b" ,
      BinExp ( Add , Variable ( NumName ( 0 ) ) ,
        Variable ( NumName ( 1 ) ) ) ) &&
    parsesAs ( Parser.exp , "a + b + c" ,
      BinExp ( Add , Variable ( NumName ( 0 ) ) ,
        BinExp ( Add , Variable ( NumName ( 1 ) ) ,
          Variable ( NumName ( 2 ) ) ) ) ) &&
    ( lexAndParse ( Parser.exp , Source fromString "a + b + c" ) ==
      lexAndParse ( Parser.exp , Source fromString "a + ( b + c )" ) ) &&
    parsesAs ( Parser.exp , "[]" , ListExp ( List.empty ) ) &&
    parsesAs ( Parser.exp , "[ 10 ]" ,
      ListExp ( List ( IntLiteral ( 10 ) ) ) ) &&
    parsesAs ( Parser.exp , "[ 10 , 0 , 12 ]" ,
      ListExp ( List ( IntLiteral ( 10 ) , IntLiteral ( 0 ) ,
        IntLiteral ( 12 ) ) ) )
  }
}

object TurnerMachineProperties extends Properties("TurnerMachineState") {
  import parser._
  import interpreter.turner.runWithTurnerMachine
  import scala.io.Source

  val names: Map[Name, String] = (((0 to 51) map (n => NumName(n)))
    .zip(((('a' to 'z') ++ ('A' to 'Z')) map (s => s.toString)))).toMap

  val next: NumName = NumName(52)

  property("simpleProcess") = {
    val procStr: String =
      "new c: a. [ receive c;; y: int. send c;;y . end | send c;; 10. end ]"
    lexAndParse ( Parser.proc , Source fromString procStr ) match {
      case Right((nmap, nn, proc)) => {
        runWithTurnerMachine(proc, nmap map (_.swap), nn)._1.listify
          .filter(_ != End)
          .head.alphaEquiv(Send(ChanLiteral(NumName(0)), List(),
            List(IntLiteral(10)), End)).nonEmpty
      }
      case Left(_) => false
    }
  }

  property("addThreeNumbers") = {

    /* proc =
     *  [ send a ; ; x . end
     *  | send a ; ; y . end
     *  | send a ; ; z . end
     *  |
     *    receive a ; ; b : int .
     *    receive a ; ; c : int .
     *    receive a ; ; d : int .
     *    send e : b + c + d . end
     *  |
     *    receive e ; ; f : int . send a : f . end
     *  ]
     *
     * should evaluate to =
     *
     *  send a : (x+y+z) . end
     */
    val proc: Function3[Int, Int, Int, Proc] = ( x: Int, y: Int, z: Int ) =>
      Proc.fromList(List(
        Send(ChanLiteral(NumName(0)), List(), List(IntLiteral(x)), End),
        Send(ChanLiteral(NumName(0)), List(), List(IntLiteral(y)), End),
        Send(ChanLiteral(NumName(0)), List(), List(IntLiteral(z)), End),
        Receive(false, ChanLiteral(NumName(0)),
          List(), List((NumName(1), SInt)),
          Receive(false, ChanLiteral(NumName(0)),
            List(), List((NumName(2), SInt)),
          Receive(false, ChanLiteral(NumName(0)),
            List(), List((NumName(3), SInt)),
          Send(ChanLiteral(NumName(4)), List(), List(BinExp(Add, BinExp(Add,
            Variable(NumName(1)), Variable(NumName(2))), Variable(NumName(3)))),
              End)))),
        Receive(false, ChanLiteral(NumName(4)), List(), List((NumName(5), SInt)),
          Send(ChanLiteral(NumName(0)), List(), List(Variable(NumName(5))),
            End))))
    Prop.forAll { ( x: Int, y: Int, z: Int ) => {
      val procPost: Proc = runWithTurnerMachine(proc(x, y, z), names, next)._1
      Proc.fromList(procPost.listify.filter( _ != End )).alphaEquiv(
        Proc.fromList(Send(ChanLiteral(NumName(0)), List(),
          List(IntLiteral(x + y + z)), End)
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
    new Typecheck(NumName(n)).dequantify(typeOfUnOp(PLeft)) ==
      SFunc(SPair(SVar(NumName(n)), SVar(NumName(n + 1))), SVar(NumName(n)))
  }}}

  property("dequantifyPRight") = Prop.forAll { ( n: Int ) => { ( n > 1 ) ==> {
    new Typecheck(NumName(n)).dequantify(typeOfUnOp(PRight)) ==
      SFunc(SPair(SVar(NumName(n)), SVar(NumName(n + 1))),
        SVar(NumName(n + 1)))
  }}}

  property("unifyBadlyTypedExpFails") = {
    val exp: Exp = UnExp(PLeft, IntLiteral(1))
    val checker: Typecheck = new Typecheck(NumName(0))
    val (_, constraints: ConstraintSet) = checker.constraintsExp(exp, Map.empty)
    checker.unify(constraints, ConstraintSet.empty).isLeft
  }

  property("unifyArbitraryExpNoCrash") = Prop.forAll { exp: Exp => {
    // Free variables are SInts in this typing environment
    val checker: Typecheck = new Typecheck(findNextName(exp.free))
    val (_, constraints: ConstraintSet) = checker.constraintsExp(exp,
      ((exp.free union exp.chanLiterals) map (n => (n, SInt))).toMap)
    checker.unify (constraints, ConstraintSet.empty)
    true
  }}

  property("unifyArbitraryProcNoCrash") = Prop.forAll { proc: Proc => {
    // Free variables are SInts in this typing environment
    val checker: Typecheck = new Typecheck(findNextName(proc.free))
    val (_, constraints: ConstraintSet) = checker.constraintsProc(proc,
      ((proc.free union proc.chanLiterals) map (n => (n, SInt))).toMap)
    checker.unify(constraints, ConstraintSet.empty)
    true
  }}

  def checks(procStr: String): Boolean =
    lexAndParse ( Parser.proc , Source fromString procStr ) match {
      case Left  ( _                    ) => false
      case Right ( ( nmap , nn , proc ) ) =>
        new Typecheck ( nn ).checkProc ( proc ).isDefined
    }
  def allCheck(procStrs: List[String]): Boolean =
    procStrs.map ( checks ).foldLeft ( true ) ( _ && _ )
  def noneCheck(procStrs: List[String]): Boolean =
    !procStrs.map ( checks ).foldLeft ( false ) ( _ || _ )

  property("reallyReallySimpleProcsTypeCheck") = allCheck ( List (
    " end "                 ,
    " [ end ] "             ,
    " [ end | end | end ] " ) )

  property("reallySimpleProcsTypeCheck") = allCheck ( List (
    " new a: @{; int}. receive a;; y: int. end " ,
    " new a: @{; int}. send a;; 12. end        " ,
    " new a: @{; bool}. end                    " ,
    " let x: int = -> (10, 11). end            " ,
    " new a: @{; int}. server a;; y: int. end  " ) )

  property("letProcChecks") = checks (
    " [ let a: int         = 10     . " +
    "   let b: bool        = true   . " +
    "   let c: char        = 'x'    . " +
    "   let d: string      = \"xy\" . " +
    "   new e: @{; int}             . " +
    "   let f: @{; int}    = e      . " +
    "   let g: (int, char) = (a, c) . " +
    "   end                           " +
    " ]                               " )

  property("simpleListsCheck") = allCheck ( List (
    " if ? [] then end else end endif                               " ,
    " new a: @{k;  k }. send a; int ; *--  [ 1 , 2 , 3 , 4   ]. end " ,
    " new a: @{k; [k]}. send a; int ; 0 :: [ 1 , 2 , 3 , 4   ]. end " ,
    " new a: @{k; [k]}. send a; int ; -**  [ 1 , 2 , 3 , 4   ]. end " ,
    " new a: @{k; [k]}. send a; char; -**  [ 'a' , 'b' , 'c' ]. end " ,
    " new a: @{k; [k]}. send a; int ;      [ 1 , 2 , 3 , 4   ]. end " ) )

  property("badListsDontCheck") = noneCheck ( List (
    " new a: @{; [int]}. send a;; true :: [ 1 , 2 , 3 , 4 ] . end " ,
    " new a: @{; [int]}. send a;; [ 1 , 2 , 3 , 4 , '5' ] . end "   ,
    " if [] then end else end endif "             ) )

  property("simpleProcTypeChecks") = checks (
    " new a: @{; @{; int}}     . " +
    " new x: @{;     int }     . " +
    " [ receive a;; y: @{;int} . " +
    "     send y;; 12          . " +
    "     end                    " +
    " | send a;; x             . " +
    "     end                    " +
    " ]                          " )

  property("badProcsDontCheck") = noneCheck ( List (
    " new a: @{;@{;int}}. new y: @{;int}.                           " +
    "   receive a;; y: @{;int } . send y;; y. end | send a: 12. end " ,
    " new a: @{;@{;int}}. new y: @{;int}.                           " +
    "   receive a;; y: @{;bool} . send y;; 3. end | send a: 12. end " ,
    " new a: @{;@{;int}}. new y: @{;int}.                           " +
    "   receive a;; y: @{;char} . send y;; y. end                   " ) )

  property("badPolyDoesntCheck") = ! checks (
    " new echo: @{d; d, @{;d}}.                            " +
    "   [ server echo; d; x:d, r: @{;d}. send r;; x. end   " +
    "   | new r: @{;int }. send echo; bool; (10  , r). end " +
    "   | new r: @{;bool}. send echo; int ; (true, r). end " +
    "   | new r: @{;char}. send echo; char; 'a'      . end " +
    "   ]                                                  " )

  property("simpleloopExampleTypechecks") =
    checks ( Source.fromFile("examples/simpleloop"         ).mkString )

  property("looping_functionsExampleTypechecks") =
    checks ( Source.fromFile("examples/looping_functions"  ).mkString )

  property("functionsExampleTypechecks") =
    checks ( Source.fromFile("examples/functions"          ).mkString )

  property("listsExampleTypechecks") =
    checks ( Source.fromFile("examples/lists"              ).mkString )

  property("strings_charsExampleTypechecks") =
    checks ( Source.fromFile("examples/strings_chars"      ).mkString )

  property("polymorphic_lengthExampleTypechecks") =
    checks ( Source.fromFile("examples/polymorphic_length" ).mkString )

  property("polymorphic_echoExampleTypechecks") =
    checks ( Source.fromFile("examples/polymorphic_echo"   ).mkString )
}

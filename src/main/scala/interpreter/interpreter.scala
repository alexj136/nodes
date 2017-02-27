package interpreter

import syntax._

abstract class MachineState {
  def toProc: Proc
  def step: Option[MachineState]
  def someOf: Option[MachineState] = Some(this)
  def getNames: Map[Name, String]
  def getNext: Name
}

sealed abstract class EvaluationException extends Exception
case class TypeError(message: String) extends EvaluationException
case class FreeVariableError(in: SyntaxElement) extends EvaluationException
case class ListBoundError(message: String, in: SyntaxElement)
  extends EvaluationException

object substituteProc extends Function3[Proc, Name, EvalExp, Proc] {

  /** Substitute the EvalExp 'to' for the Name 'from' within the Proc act, and
    *  obtain the resulting Proc.
    */
  def apply(p: Proc, from: Name, to: EvalExp): Proc = {
    val subP : Function[Proc, Proc] = q => substituteProc(q, from, to)
    val subE : Function[Exp, Exp] = e => substituteExp(e, from, to)
    p match {

      case Send(ch, msg, q) => Send(subE(ch), subE(msg), subP(q))

      case Receive(repl, ch, bind, ty, q) => {
        val newQ = if (bind == from) q else subP(q)
        Receive(repl, subE(ch), bind, ty, newQ)
      }

      case LetIn(name, ty, exp, q) => {
        val newQ = if (name == from) q else subP(q)
        LetIn(name, ty, subE(exp), newQ)
      }

      case IfThenElse(exp, tP, fP) =>
        IfThenElse(subE(exp), subP(tP), subP(fP))

      case Parallel(q, r) => Parallel(subP(q), subP(r))

      case New(name, ty, q) =>
        New(name, ty, if(name == from) q else subP(q))

      case End => End
    }
  }
}

object substituteExp extends Function3[Exp, Name, EvalExp, Exp] {

  def apply(exp: Exp, from: Name, to: EvalExp): Exp = {
    val subE : Exp => Exp = e => substituteExp(e, from, to)
    exp match {
      case Variable    ( n ) if n == from => to.unEvalExp
      case Variable    ( n ) if n != from => exp
      case IntLiteral  ( x )              => exp
      case BoolLiteral ( x )              => exp
      case KharLiteral ( c )              => exp
      case ChanLiteral ( c )              => exp
      case Pair        ( l  , r         ) => Pair(subE(l), subE(r))
      case UnExp       ( ty , e         ) => UnExp(ty, subE(e))
      case BinExp      ( ty , lhs , rhs ) => BinExp(ty, subE(lhs), subE(rhs))
      case ListExp     ( es             ) => ListExp(es map subE)
    }
  }
}

sealed abstract class EvalExp {
  def unEvalExp: Exp = this match {
    case EEInt  ( value  ) => IntLiteral  ( value                     )
    case EEBool ( value  ) => BoolLiteral ( value                     )
    case EEKhar ( value  ) => KharLiteral ( value                     )
    case EEChan ( name   ) => ChanLiteral ( name                      )
    case EEPair ( l , r  ) => Pair        ( l.unEvalExp , r.unEvalExp )
    case EEList ( es     ) => ListExp     ( es map ( _.unEvalExp )    )
  }
  def channelName: Name = this match {
    case EEInt  ( _     ) => throw TypeError("integer")
    case EEBool ( _     ) => throw TypeError("boolean")
    case EEKhar ( _     ) => throw TypeError("character")
    case EEChan (name   ) => name
    case EEPair ( _ , _ ) => throw TypeError("pair")
    case EEList ( _     ) => throw TypeError("list")
  }
  def channelNames: Set[Name] = this match {
    case EEInt  ( _       ) => Set.empty
    case EEBool ( _       ) => Set.empty
    case EEKhar ( _       ) => Set.empty
    case EEChan ( name    ) => Set(name)
    case EEPair ( l , r   ) => l.channelNames union r.channelNames
    case EEList ( Nil     ) => Set.empty
    case EEList ( e :: es ) => e.channelNames union EEList(es).channelNames
  }
  def isEEChan: Boolean = this match { case EEChan(_) => true; case _ => false }
}
case class EEInt  ( value: Int                    ) extends EvalExp
case class EEBool ( value: Boolean                ) extends EvalExp
case class EEKhar ( value: Char                   ) extends EvalExp
case class EEChan ( name:  Name                   ) extends EvalExp
case class EEPair ( lhs:   EvalExp , rhs: EvalExp ) extends EvalExp
case class EEList ( exps:  List[EvalExp]          ) extends EvalExp

object EvalExp {

  def from(exp: Exp): EvalExp = exp match {
    case Variable    ( n     ) => throw FreeVariableError(Variable(n))
    case IntLiteral  ( x     ) => EEInt(x)
    case BoolLiteral ( x     ) => EEBool(x)
    case KharLiteral ( c     ) => EEKhar(c)
    case ChanLiteral ( c     ) => EEChan(c)
    case Pair        ( l , r ) => EEPair(EvalExp from l, EvalExp from r)
    case ListExp     ( es    ) => EEList(es map (EvalExp from _))
    case UnExp ( ty, e ) => (ty, EvalExp from e) match {

      case (Not, EEBool(b)) => EEBool(!b)
      case (Not, _) => throw TypeError("!")

      case (PLeft, EEPair(l, _)) => l
      case (PLeft, _           ) => throw TypeError("<-")

      case (PRight, EEPair(_, r)) => r
      case (PRight, _           ) => throw TypeError("->")

      case (Empty, EEList(Nil)) => EEBool(true)
      case (Empty, EEList(_  )) => EEBool(false)
      case (Empty, _          ) => throw TypeError("?")

      case (Head, EEList(Nil   )) => throw ListBoundError("*-- []", exp)
      case (Head, EEList(e :: _)) => e
      case (Head, _             ) => throw TypeError("*--")

      case (Tail, EEList(Nil    )) => throw ListBoundError("-** []", exp)
      case (Tail, EEList(_ :: es)) => EEList(es)
      case (Tail, _              ) => throw TypeError("-**")
    }
    case BinExp ( ty , lhs , rhs ) =>
        (ty, EvalExp from lhs, EvalExp from rhs) match {

      // Int -> Int -> Int
      case(Add, EEInt(l), EEInt(r)) => EEInt(l + r)
      case(Add, _, _) => throw TypeError("+")

      case(Sub, EEInt(l), EEInt(r)) => EEInt(l - r)
      case(Sub, _, _) => throw TypeError("-")

      case(Mul, EEInt(l), EEInt(r)) => EEInt(l * r)
      case(Mul, _, _) => throw TypeError("*")

      case(Div, EEInt(l), EEInt(r)) => EEInt(l / r)
      case(Div, _, _) => throw TypeError("/")

      case(Mod, EEInt(l), EEInt(r)) => EEInt(l % r)
      case(Mod, _, _) => throw TypeError("%")

      // A -> A -> Bool
      case(Equal, EEInt(l), EEInt(r)) => EEBool(l == r)
      case(Equal, EEBool(l), EEBool(r)) => EEBool(l == r)
      case(Equal, EEChan(l), EEChan(r)) => EEBool(l == r)
      case(Equal, _, _) => throw TypeError("==")

      case(NotEqual, EEInt(l), EEInt(r)) => EEBool(l != r)
      case(NotEqual, EEBool(l), EEBool(r)) => EEBool(l != r)
      case(NotEqual, EEChan(l), EEChan(r)) => EEBool(l != r)
      case(NotEqual, _, _) => throw TypeError("!=")

      // Int -> Int -> Bool
      case(Less, EEInt(l), EEInt(r)) => EEBool(l < r)
      case(Less, _, _) => throw TypeError("<")

      case(LessEq, EEInt(l), EEInt(r)) => EEBool(l <= r)
      case(LessEq, _, _) => throw TypeError("<=")

      case(Greater, EEInt(l), EEInt(r)) => EEBool(l > r)
      case(Greater, _, _) => throw TypeError(">")

      case(GreaterEq, EEInt(l), EEInt(r)) => EEBool(l >= r)
      case(GreaterEq, _, _) => throw TypeError(">=")

      // Bool -> Bool -> Bool
      case(And, EEBool(l), EEBool(r)) => EEBool(l && r)
      case(And, _, _) => throw TypeError(">")

      case(Or, EEBool(l), EEBool(r)) => EEBool(l || r)
      case(Or, _, _) => throw TypeError(">=")

      case(Cons, e, EEList(es)) => EEList(e :: es)
      case(Cons, _, _         ) => throw TypeError("::")
    }
  }
}

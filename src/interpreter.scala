package interpreter

import syntax._

object Evaluator {

  abstract class MachineState {
    def toProc: Proc
    def someOf: Option[MachineState] = Some(this)
    def step: Option[MachineState]
  }

  class TurnerMachineState(
      run:   List[Proc], 
      wait:  Map[Name, List[Proc]],
      names: Map[Name, String],
      next:  Name) extends MachineState {

    override def toProc: Proc = (this.run :: this.wait.toList.map(_._2)).flatten
      .fold(End){ (p, q) => Parallel(p, q) }

    def withRun(newRun: List[Proc]): TurnerMachineState =
      new TurnerMachineState(newRun, this.wait, this.names, this.next)

    def withWait(ch: Name, onCh: List[Proc]): TurnerMachineState =
      new TurnerMachineState(
        this.run, this.wait.updated(ch, onCh), this.names, this.next)

    def withNext(newNext: Name): TurnerMachineState =
      new TurnerMachineState(this.run, this.wait, this.names, newNext)

    override def step: Option[MachineState] = this.run match {
      case Nil => None

      case Send(ChanLiteral(ch), msg, p) :: runTail
        if this.names.get(ch) == Some("$print") => {
          println(evalExp(msg).unEvalExp pstr this.names)
          this.withRun(p :: runTail).someOf
        }

      case Send(ChanLiteral(ch), msg, p) :: runTail => this.wait(ch) match {

        case Receive(repl, ChanLiteral(_), bind, q) :: moreRecs => {
          val qSub: Proc = substituteProc(q, bind, evalExp(msg))
          val waitTail: List[Proc] =
            if (repl) List(Receive(true, ChanLiteral(ch), bind, q)) else Nil
          this.withRun(p :: (runTail :+ qSub))
              .withWait(ch, moreRecs ++ waitTail)
              .someOf
        }
        case nilOrSends =>
          this.withRun(runTail)
              .withWait(ch, nilOrSends :+ Send(ChanLiteral(ch), msg, p))
              .someOf
      }

      case Send(chExp, msg, p) :: runTail => evalExp(chExp) match {
        case EEChan(ch) =>
          this.withRun(Send(ChanLiteral(ch), msg, p) :: runTail).someOf
        case _          => throw FreeVariableError(Send(chExp, msg, p))
      }

      case Receive(repl, ChanLiteral(ch), bind, p) :: runTail =>
        this.wait(ch) match {

          case Send(ChanLiteral(_), msg, q) :: moreSends => {
            val pSub: Proc = substituteProc(p, bind, evalExp(msg))
            val newRun: List[Proc] =
              if (repl)
                Receive(true, ChanLiteral(ch), bind, p) ::
                  (runTail :+ pSub :+ q)
              else
                pSub :: (runTail :+ q)
            this.withRun(newRun)
                .withWait(ch, moreSends)
                .someOf
          }
          case nilOrReceives =>
            this.withRun(runTail)
                .withWait(ch,
                  nilOrReceives :+ Receive(repl, ChanLiteral(ch), bind, p))
                .someOf
        }

      case Receive(rep, chExp, bind, p) :: runTail => evalExp(chExp) match {
        case EEChan(ch) =>
          this.withRun(Receive(rep, ChanLiteral(ch), bind, p) :: runTail).someOf
        case _          => throw FreeVariableError(Receive(rep, chExp, bind, p))
      }

      case LetIn(name, exp, p) :: runTail =>
        this.withRun(substituteProc(p, name, evalExp(exp)) :: runTail).someOf

      case IfThenElse(exp, tP, fP) :: runTail => evalExp(exp) match {
        case EEBool(true) => this.withRun(tP :: runTail).someOf
        case EEBool(false) => this.withRun(fP :: runTail).someOf
        case _ => throw TypeError("if")
      }

      case Parallel(p, q) :: runTail =>
        this.withRun(p :: (runTail :+ q)).someOf

      case New(name, p) :: runTail => {
        val nu: Name = this.next.next
        val newP: Proc = substituteProc(p, name, EEChan(nu))
        this.withWait(nu, Nil).withRun(newP :: runTail).withNext(nu).someOf
      }

      case End :: runTail => this.withRun(runTail).someOf
    }
  }

  sealed abstract class EvalExp {
    def unEvalExp: Exp = this match {
      case EEInt  ( value  ) => IntLiteral  ( value                     )
      case EEBool ( value  ) => BoolLiteral ( value                     )
      case EEChan ( name   ) => ChanLiteral ( name                      )
      case EEPair ( l , r  ) => Pair        ( l.unEvalExp , r.unEvalExp )
    }
    def channelName: Name = this match {
      case EEInt  (value  ) => throw TypeError("integer")
      case EEBool (value  ) => throw TypeError("boolean")
      case EEChan (name   ) => name
      case EEPair ( _ , _ ) => throw TypeError("pair")
    }
  }
  case class EEInt  ( value: Int                    ) extends EvalExp
  case class EEBool ( value: Boolean                ) extends EvalExp
  case class EEChan ( name:  Name                   ) extends EvalExp
  case class EEPair ( lhs:   EvalExp , rhs: EvalExp ) extends EvalExp

  /** Substitute the Name 'to' for the Name 'from' within the Proc act, and
    *  obtain the resulting Proc.
    */
  def substituteProc(p: Proc, from: Name, to: EvalExp): Proc = {
    val subP : Function[Proc, Proc] = q => substituteProc(q, from, to)
    val subE : Function[Exp, Exp] = e => substituteExp(e, from, to)
    p match {

      case Send(ch, msg, q) => Send(subE(ch), subE(msg), subP(q))

      case Receive(repl, ch, bind, q) => {
        val newQ = if (bind == from) q else subP(q)
        Receive(repl, subE(ch), bind, newQ)
      }

      case LetIn(name, exp, q) => {
        val newQ = if (name == from) q else subP(q)
        LetIn(name, subE(exp), newQ)
      }

      case IfThenElse(exp, tP, fP) =>
        IfThenElse(subE(exp), subP(tP), subP(fP))

      case Parallel(q, r) => Parallel(subP(q), subP(r))

      case New(name, q) =>
        New(name, if(name == from) q else subP(q))

      case End => End
    }
  }

  def substituteExp(exp: Exp, from: Name, to: EvalExp): Exp = {
    val subE : Function[Exp, Exp] = e => substituteExp(e, from, to)
    exp match {
      case Variable    ( n ) if n == from => to.unEvalExp
      case Variable    ( n ) if n != from => exp
      case IntLiteral  ( x )              => exp
      case BoolLiteral ( x )              => exp
      case ChanLiteral ( c )              => exp
      case Pair        ( l  , r         ) => Pair(subE(l), subE(r))
      case UnExp       ( ty , e         ) => UnExp(ty, subE(e))
      case BinExp      ( ty , lhs , rhs ) => BinExp(ty, subE(lhs), subE(rhs))
    }
  }

  def evalExp(exp: Exp): EvalExp = exp match {
    case Variable    ( n     ) => throw FreeVariableError(Variable(n))
    case IntLiteral  ( x     ) => EEInt(x)
    case BoolLiteral ( x     ) => EEBool(x)
    case ChanLiteral ( c     ) => EEChan(c)
    case Pair        ( l , r ) => EEPair(evalExp(l), evalExp(r))
    case UnExp ( ty, e ) => (ty, evalExp(e)) match {

      case (Not, EEBool(b)) => EEBool(!b)
      case (Not, _) => throw TypeError("!")

      case (PLeft, EEPair(l, _)) => l
      case (PLeft, _           ) => throw TypeError("<-")

      case (PRight, EEPair(_, r)) => r
      case (PRight, _           ) => throw TypeError("->")
    }
    case BinExp ( ty , lhs , rhs ) => (ty, evalExp(lhs), evalExp(rhs)) match {

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
    }
  }

  sealed abstract class EvaluationException extends Exception
  case class TypeError(message: String) extends EvaluationException
  case class FreeVariableError(in: SyntaxElement) extends EvaluationException
}

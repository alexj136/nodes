package object Interpreter {

  import Syntax.Syntax._

  object Interpreter {

    type MachineState = (List[Process], Map[Name, List[Process]])

    sealed abstract class EvalExp
    case class EEInt(value: Int) extends EvalExp
    case class EEBool(value: Boolean) extends EvalExp
    case class EEChan(name: Name) extends EvalExp

    def unEvalExp(e: EvalExp): Expression = e match {
      case EEInt(value) => IntLiteral(value)
      case EEBool(value) => BoolLiteral(value)
      case EEChan(name) => ChanLiteral(name)
    }

    def channelName(e: EvalExp): Name = e match {
      case EEInt(value) =>
        throw new RuntimeException("Type error: an int is not a channel")
      case EEBool(value) =>
        throw new RuntimeException("Type error: a bool is not a channel")
      case EEChan(name) => name
    }

    def runProc(p: Process): Process = p match {
      case Send(_, _, _) => ???
      case Receive(_, _, _) => ???
      case RepRec(_, _, _) => ???
      case LetIn(name, exp, q) => ???
      case IfThenElse(exp, tP, fP) => ???
      case Parallel(q, r) => ???
      case End => End
    }

    def step(state: MachineState): Option[MachineState] = state match {
      case (Nil, _) => None
      case (rq, wm) => {
        val newRQ = rq match {

          case Parallel(p, q) :: rqt => (p :: (rqt :+ q), wm)

          case LetIn(n, e, q) :: rqt =>
            (substituteProc(q, n, evalExp(e)) :: rqt, wm)
        }
        Some(newRQ)
      }
    }

    /** Substitute the Name 'to' for the Name 'from' within the Process act, and
     *  obtain the resulting Process.
     */
    def substituteProc(p: Process, from: Name, to: EvalExp): Process = {
      val subP : Function[Process, Process] =
        q => substituteProc(q, from, to)
      val subE : Function[Expression, Expression] =
        e => substituteExp(e, from, to)
      p match {

        case Send(ch, msg, q) => {
          val newCh = if (ch == from) channelName(to) else ch
          Send(newCh, subE(msg), subP(q))
        }

        case Receive(ch, bind, q) => {
          val newCh = if (ch == from) channelName(to) else ch
          val newQ = if (bind == from) q else subP(q)
          Receive(newCh, bind, newQ)
        }

        case RepRec(ch, bind, q) => {
          val newCh = if (ch == from) channelName(to) else ch
          val newQ = if (bind == from) q else subP(q)
          RepRec(newCh, bind, newQ)
        }

        case LetIn(name, exp, q) => {
          val newQ = if (name == from) q else subP(q)
          LetIn(name, subE(exp), newQ)
        }

        case IfThenElse(exp, tP, fP) =>
          IfThenElse(subE(exp), subP(tP), subP(fP))

        case Parallel(q, r) => Parallel(subP(q), subP(r))

        case End => End
      }
    }

    def substituteExp(exp: Expression, from: Name, to: EvalExp): Expression = {
      val subE : Function[Expression, Expression] =
        e => substituteExp(e, from, to)
      exp match {
        case Variable(n) if n == from => unEvalExp(to)
        case Variable(n) if n != from => exp
        case IntLiteral(x) => exp
        case BoolLiteral(x) => exp
        case ChanLiteral(c) => exp
        case Not(e) => Not(subE(e))
        case BinExp(ty, lhs, rhs) => BinExp(ty, subE(lhs), subE(rhs))
      }
    }

    def evalExp(exp: Expression): EvalExp = exp match {
      case Variable(n) =>
        throw new RuntimeException(s"Free variable error: '${n}'.")
      case IntLiteral(x) => EEInt(x)
      case BoolLiteral(x) => EEBool(x)
      case ChanLiteral(c) => EEChan(c)
      case Not(e) => evalExp(e) match {
        case EEBool(b) => EEBool(!b)
        case _ => throw new RuntimeException("Type error in negation")
      }
      case BinExp(ty, lhs, rhs) => (ty, evalExp(lhs), evalExp(rhs)) match {

        // Int -> Int -> Int
        case(Add, EEInt(l), EEInt(r)) => EEInt(l + r)
        case(Add, _, _) =>
          throw new RuntimeException("Type error in +")

        case(Sub, EEInt(l), EEInt(r)) => EEInt(l - r)
        case(Sub, _, _) =>
          throw new RuntimeException("Type error in -")

        case(Mul, EEInt(l), EEInt(r)) => EEInt(l * r)
        case(Mul, _, _) =>
          throw new RuntimeException("Type error in *")

        case(Div, EEInt(l), EEInt(r)) => EEInt(l / r)
        case(Div, _, _) =>
          throw new RuntimeException("Type error in /")

        case(Mod, EEInt(l), EEInt(r)) => EEInt(l % r)
        case(Mod, _, _) =>
          throw new RuntimeException("Type error in %")

        // A -> A -> Bool
        case(Equal, EEInt(l), EEInt(r)) => EEBool(l == r)
        case(Equal, EEBool(l), EEBool(r)) => EEBool(l == r)
        case(Equal, EEChan(l), EEChan(r)) => EEBool(l == r)
        case(Equal, _, _) =>
          throw new RuntimeException("Type error in ==")

        case(NotEqual, EEInt(l), EEInt(r)) => EEBool(l != r)
        case(NotEqual, EEBool(l), EEBool(r)) => EEBool(l != r)
        case(NotEqual, EEChan(l), EEChan(r)) => EEBool(l != r)
        case(NotEqual, _, _) =>
          throw new RuntimeException("Type error in !=")

        // Int -> Int -> Bool
        case(Less, EEInt(l), EEInt(r)) => EEBool(l < r)
        case(Less, _, _) =>
          throw new RuntimeException("Type error in <")

        case(LessEq, EEInt(l), EEInt(r)) => EEBool(l <= r)
        case(LessEq, _, _) =>
          throw new RuntimeException("Type error in <=")

        case(Greater, EEInt(l), EEInt(r)) => EEBool(l > r)
        case(Greater, _, _) =>
          throw new RuntimeException("Type error in >")

        case(GreaterEq, EEInt(l), EEInt(r)) => EEBool(l >= r)
        case(GreaterEq, _, _) =>
          throw new RuntimeException("Type error in >=")

        // Bool -> Bool -> Bool
        case(And, EEBool(l), EEBool(r)) => EEBool(l && r)
        case(And, _, _) =>
          throw new RuntimeException("Type error in >")

        case(Or, EEBool(l), EEBool(r)) => EEBool(l || r)
        case(Or, _, _) =>
          throw new RuntimeException("Type error in >=")
      }
    }
  }
}

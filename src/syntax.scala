package Syntax {
  object Syntax {

    class Name(value: String)
    class Program(processes: List[Restrict])
    class Restrict(names: Set[Name], processes: List[Process])
    class Process(repeats: Boolean, action: Action)

    sealed abstract class Action {
      def isSending(): Boolean = this match {
        case Send(_, _, _) => true
        case _ => false
      }
      def isReceiving(): Boolean = this match {
        case Receive(_, _, _) => true
        case _ => false
      }
    }
    case class Send(chan: Name, msg: Expression, next: Action) extends Action
    case class Receive(chan: Name, bind: Expression, next: Action)
      extends Action
    case class LetIn(name: Name, exp: Expression, next: Action) extends Action
    case class IfThenElse(exp: Expression, tAct: Action, fAct: Action)
      extends Action
    case object End extends Action

    sealed abstract class Expression
    case class Variable(name: Name) extends Expression
    case class IntLiteral(value: Int) extends Expression
    case class BoolLiteral(value: Boolean) extends Expression
    case class ChanLiteral(name: Name) extends Expression
    case class BinExp(binOpType: BinOp, lhs: Expression, rhs: Expression)
      extends Expression
    case class Not(of: Expression) extends Expression

    sealed abstract class BinOp
    case object Add extends BinOp
    case object Sub extends BinOp
    case object Mul extends BinOp
    case object Div extends BinOp
    case object Mod extends BinOp

    case object Equal extends BinOp
    case object NotEqual extends BinOp

    case object Less extends BinOp
    case object LessEq extends BinOp
    case object Greater extends BinOp
    case object GreaterEq extends BinOp

    case object And extends BinOp
    case object Or extends BinOp
  }
}

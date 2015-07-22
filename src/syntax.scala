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
    case class Receive(chan: Name, bind: Expression, next: Action) extends Action
    case class LetIn(name: Name, exp: Expression, next: Action) extends Action
    case class IfThenElse(exp: Expression, tNext: Action, fNext: Action) extends Action
    case object End extends Action

    sealed abstract class Expression
    case class Variable(name: Name) extends Expression
    case class IntLiteral(value: Int) extends Expression
    case class BoolLiteral(value: Boolean) extends Expression
    case class ChanLiteral(name: Name) extends Expression
    case class BinExp(binOpType: BinOpType, lhs: Expression, rhs: Expression)
      extends Expression

    sealed abstract class BinOpType
    case object Add extends BinOpType
    case object Sub extends BinOpType
    case object Mul extends BinOpType
    case object Div extends BinOpType
    case object Mod extends BinOpType
    case object And extends BinOpType
    case object Or extends BinOpType
  }
}

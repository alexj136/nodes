package Syntax {
  object Syntax {

    class Name(id: Int) {
      def next: Name = new Name(this.id + 1)
    }

    sealed abstract class Process
    case class Send(ch: Name, msg: Expression, p: Process) extends Process
    case class Receive(repl: Boolean, ch: Name, bind: Name, p: Process)
      extends Process
    case class LetIn(name: Name, exp: Expression, p: Process) extends Process
    case class IfThenElse(exp: Expression, tP: Process, fP: Process)
      extends Process
    case class Parallel(p: Process, q: Process) extends Process
    case class Restrict(name: Name, p: Process) extends Process
    case object End extends Process

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

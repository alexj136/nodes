package syntax

class Name(id: Int) {
  def next: Name = new Name(this.id + 1)
}

sealed abstract class Proc
case class Send(ch: Name, msg: Exp, p: Proc) extends Proc
case class Receive(repl: Boolean, ch: Name, bind: Name, p: Proc) extends Proc
case class LetIn(name: Name, exp: Exp, p: Proc) extends Proc
case class IfThenElse(exp: Exp, tP: Proc, fP: Proc) extends Proc
case class Parallel(p: Proc, q: Proc) extends Proc
case class Restrict(name: Name, p: Proc) extends Proc
case object End extends Proc

sealed abstract class Exp
case class Variable(name: Name) extends Exp
case class IntLiteral(value: Int) extends Exp
case class BoolLiteral(value: Boolean) extends Exp
case class ChanLiteral(name: Name) extends Exp
case class BinExp(binOpType: BinOp, lhs: Exp, rhs: Exp) extends Exp
case class Not(of: Exp) extends Exp

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

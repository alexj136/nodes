type Name = String
type Prog = Map[Name, Statement]
abstract class Statement
case class Skip extends Statement
case class Input(name: Name, node: Name) extends Statement
case class Output(exp: Expression, node: Name) extends Statement
case class While(exp: Expression, stmts: List[Statement]) extends Statement
case class Assign(name: Name, exp: Expression) extends Statement
abstract class Expression
case class Variable(name: Name) extends Expression
case class EmptyList extends Expression
case class Cons(hd: Expression, tl: Expression) extends Expression
case class Head(exp: Expression) extends Expression
case class Tail(exp: Expression) extends Expression
type Store = Map[Name, Expression]
type Context = Map[Name, ( , Store)]
object Nodes {
  def evalExpression(store: Store, node: Name, exp: Expression): Expression =
    exp match {
      case Variable(varName) => store(node)(varName)
      case EmptyList => exp
      case Cons(a, b) => Cons(evalExpression(a), evalExpression(b))
      case Head(EmptyList) => EmptyList
      case Head(Cons(hd, tl)) => evalExpression(hd)
      case Head(other) => evalExpression(Head(evalExpression(other)))
      case Tail(EmptyList) => EmptyList
      case Tail(Cons(hd, tl)) => evalExpression(tl)
      case Tail(other) => evalExpression(Tail(evalExpression(other)))
  }
  def evalStatement(ctx: Context, node: Name, stmt: Statement): Context =
    stmt match {
      case Skip => ctx
      case Input(name, node) =>
        evalStatement(
    }
}

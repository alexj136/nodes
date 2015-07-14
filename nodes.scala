type Name = String
type Prog = Map[Name, List[Statement]]
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
type MessageQueue = Map[Name, Expression]
class Context(map: Map[Name, (List[Statement], MessageQueue, Store)]) {
  def statementGet(nodeName: Name): (Context, Statement) =
  def statementPut(nodeName: Name, stmt: Statement): Context =
  def messageGet(rcvrName: Name, sndrName: Name): (Context, Expression) =
  def messagePut(rcvrName: Name, sndrName: Name, msg: Expression): Context =
  def varLookup(nodeName: Name, varName: Name): Expression =
  def varInsert(nodeName: Name, varName: Name, exp: Expression): Context =
}
object Nodes {
  def evalExpression(ctx: Context, exp: Expression): Expression =
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
  def evalStatement(ctx: Context, curNode: Name, stmt: Statement): Context =
    stmt match {
      case Skip => ctx
      case Input(varName, sndrNode) =>
        ctx(curNode) match { case (stmts, msgQ, store) => {
          msgQ(sndrNode) match {
            case Nil => {
              val (optionNextStmt, restStmts) = ctx(sndrNode)._1.splitAtHead
              val newCtx = ctx + (name, (stmt :: stmts, msgQ, store))
                + (sndrNode, (restStmts, ))
            }
            case msgQHead :: msgQTail => {
              val newCtx = ctx + (name, (stmts, msgQ + (sndrNode, msgQTail), store))
              evalStatement(newCtx, curNode, Assign(varName, msgQHead))
            }
          }
        }
      }
      case Output(exp, name) =>
        val newMsgQ = ctx(name)._2 ++ exp
        // update message queue for receiver node in context with evalExp
      case While(exp, stmts) =>
        val evalExp = evalExpression(ctx, exp)
        if (evalExp.equals(EmptyList))
          ctx
        else
          // update statement queue for current node with tail of loop body list
          // recursively call evalStatement for the head of the loop body list
      case Assign(name, exp) =>
        val evalExp = evalExpression(ctx, exp)
        // update store for current node with evalExp
    }
}

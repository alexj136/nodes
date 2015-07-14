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
type MessageQueue = Map[Name, List[Expression]]

class Context(map: Map[Name, (List[Statement], MessageQueue, Store)]) {
  def statementGet(nodeName: Name): Option[(Context, Statement)] = 
    this.map.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((Nil, _, _)) => None
      case Some((stmt :: stmts, msgQ, store)) =>
        Some((Context(this.map + (nodeName, (stmts, msgQ, store))), stmt))
    }
  def statementPut(nodeName: Name, stmt: Statement): Context =
    this.map.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) =>
        Context(this.map + (nodeName, (stmt :: stmts, msgQ, store)))
    }
  def messageGet(rcvrName: Name, sndrName: Name): Option[(Context, Expression)] =
    this.map.get(rcvrName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) => msgQ.get(sndrName) match {
        case None => throw new
          RuntimeException("Node name not in message queue")
        case Some(Nil) => None
        case Some(msg :: msgs) => Some((Context(this.map +
          (rcvrName, (stmts, msgQ + (sndrName, msgs), store))), msg))
      }
    }
  def messagePut(rcvrName: Name, sndrName: Name, msg: Expression): Context =
    this.map.get(rcvrName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) => msgQ.get(sndrName) match {
        case None => throw new
          RuntimeException("Node name not in message queue")
        case Some(msgs) => Context(this.map +
          (rcvrName, (stmts, msgQ + (sndrName, msgs ++ List(msg)), store)))
      }
    }
  def varGet(nodeName: Name, varName: Name): Expression =
    this.map.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((_, _, store)) => store.get(varName) match {
        case None => EmptyList
        case Some(exp) => exp
      }
    }
  def varPut(nodeName: Name, varName: Name, exp: Expression): Context =
    this.map.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) =>
        Context(this.map + (nodeName, (stmts, msgQ, store + (varName, exp))))
    }
}

object Nodes {

  def evalExpression(ctx: Context, curNode: Name, exp: Expression): Expression =
    exp match {
      case Variable(varName) => ctx.varGet(curNode, varName)
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

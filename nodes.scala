type Name = String

abstract class Expression
case class Variable(name: Name) extends Expression
case class EmptyList() extends Expression
case class Cons(hd: Expression, tl: Expression) extends Expression
case class Head(exp: Expression) extends Expression
case class Tail(exp: Expression) extends Expression

abstract class Statement
case class Skip() extends Statement
case class Input(name: Name, node: Name) extends Statement
case class Output(exp: Expression, node: Name) extends Statement
case class While(exp: Expression, stmts: List[Statement]) extends Statement
case class Assign(name: Name, exp: Expression) extends Statement

type Prog = Map[Name, Statement]
type Store = Map[Name, Expression]
type MessageQueue = Map[Name, List[Expression]]
type ContextMap = Map[Name, (List[Statement], MessageQueue, Store)]

class Context(cmap: ContextMap) {

  override def toString(): String = this.cmap.toString()

  def statementGet(nodeName: Name): Option[(Context, Statement)] = 
    this.cmap.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((Nil, _, _)) => None
      case Some((stmt :: stmts, msgQ, store)) =>
        Some((new Context(this.cmap +
          ((nodeName, (stmts, msgQ, store)))), stmt))
    }

  def statementsPut(nodeName: Name, stmts: List[Statement]): Context =
    this.cmap.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((oldStmts, msgQ, store)) =>
        new Context(this.cmap + ((nodeName, (stmts ++ oldStmts, msgQ, store))))
    }

  def messageGet(rcvrName: Name, sndrName: Name): Option[(Context, Expression)] =
    this.cmap.get(rcvrName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) => msgQ.get(sndrName) match {
        case None =>
          throw new RuntimeException("Node name not in message queue")
        case Some(Nil) => None
        case Some(msg :: msgs) =>
          Some((new Context(this.cmap +
            ((rcvrName, (stmts, msgQ + ((sndrName, msgs)), store)))), msg))
      }
    }

  def messagePut(rcvrName: Name, sndrName: Name, msg: Expression): Context =
    this.cmap.get(rcvrName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) => msgQ.get(sndrName) match {
        case None =>
          throw new RuntimeException("Node name not in message queue")
        case Some(msgs) =>
          new Context(this.cmap +
            ((rcvrName, (stmts, msgQ + ((sndrName, msgs :+ msg)), store))))
      }
    }

  def varGet(nodeName: Name, varName: Name): Expression =
    this.cmap.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((_, _, store)) => store.get(varName) match {
        case None => EmptyList()
        case Some(exp) => exp
      }
    }

  def varPut(nodeName: Name, varName: Name, exp: Expression): Context =
    this.cmap.get(nodeName) match {
      case None => throw new RuntimeException("Node name not in context")
      case Some((stmts, msgQ, store)) =>
        new Context(this.cmap +
          ((nodeName, (stmts, msgQ, store + ((varName, exp))))))
    }

  def nodesWithStatements: Set[Name] = {
    def hasMoreStatements(
      cmapEntry: (Name, (List[Statement], MessageQueue, Store))): Boolean =
        cmapEntry match {
          case (_, (Nil, _, _)) => false
          case (_, (_, _, _)) => true
        }
    this.cmap.filter(e => hasMoreStatements(e)).keySet
  }
}

object Nodes {

  def evalExpression(ctx: Context, curNode: Name, exp: Expression): Expression =
    exp match {
      case Variable(varName) => ctx.varGet(curNode, varName)
      case EmptyList() => exp
      case Cons(a, b) =>
        Cons(evalExpression(ctx, curNode, a), evalExpression(ctx, curNode, b))
      case Head(EmptyList()) => EmptyList()
      case Head(Cons(hd, tl)) => evalExpression(ctx, curNode, hd)
      case Head(other) =>
        evalExpression(ctx, curNode, Head(evalExpression(ctx, curNode, other)))
      case Tail(EmptyList()) => EmptyList()
      case Tail(Cons(hd, tl)) => evalExpression(ctx, curNode, tl)
      case Tail(other) =>
        evalExpression(ctx, curNode, Tail(evalExpression(ctx, curNode, other)))
  }

  def evalStatement(ctx: Context, curNode: Name, stmt: Statement): Context =
    stmt match {
      case Skip() => continueOrSwitch(ctx, curNode)
      case Input(varName, sndrNode) => ctx.messageGet(curNode, sndrNode) match {
        case None => continueOrSwitch(ctx, sndrNode)
        case Some((newCtx, exp)) =>
          evalStatement(newCtx, curNode, Assign(varName, exp))
      }
      case Output(exp, rcvrName) =>
        continueOrSwitch(ctx.messagePut(rcvrName, curNode, exp), curNode)
      case While(exp, stmts) =>
        if (evalExpression(ctx, curNode, exp) == EmptyList())
          continueOrSwitch(ctx, curNode)
        else
          continueOrSwitch(ctx.statementsPut(curNode, stmts :+ stmt), curNode)
      case Assign(varName, exp) =>
        ctx.varPut(curNode, varName, evalExpression(ctx, curNode, exp))
    }

  /** Continue execution on the current node if there are statements left to
   *  execute. Otherwise, arbitrarily choose another node that does have
   *  statements left, and continue execution there.
   */
  def continueOrSwitch(ctx: Context, curNode: Name): Context =
    ctx.statementGet(curNode) match {
      case Some((newCtx, nextStmt)) =>
        evalStatement(newCtx, curNode, nextStmt)
      case None => ctx.nodesWithStatements.headOption match {
        case Some(newNode) => continueOrSwitch(ctx, newNode)
        case None => ctx
      }
    }
}

object Interpreter {

  def createContext(prog: Prog): Context = {
    new Context(prog.mapValues({s => (List(s),
      prog.keySet.map({n => (n, List(): List[Expression])}).toMap, Map.empty)}))
  }

  def interpret(prog: Prog): Context = {
    val initialContext: Context = createContext(prog)
    val finalContext: Context = Nodes.continueOrSwitch(initialContext, "main")
    finalContext
  }
}

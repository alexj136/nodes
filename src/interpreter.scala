package object Interpreter {

  import Syntax.Syntax._

  class Channel(name: Name)

  class Store(map: Map[Name, Either[Channel, Tree]]) {

    def evalExp(exp: Expression): Tree = exp match {
      case AtomLit() => Right(Atom())
      case Head(e) => this.evalExp(e) match {
        case Right(Atom()) => Right(Atom())
        case Right(Cons(l, _)) => Right(l)
        case _ => throw new RuntimeException("Type error")
      }
      case Tail(e) => this.evalExp(e) match {
        case Right(Atom()) => Right(Atom())
        case Right(Cons(_, r)) => Right(r)
        case _ => throw new RuntimeException("Type error")
      }
      case ConsLit(l, r) => (this.evalExp(l), this.evalExp(r)) match {
        case (Right(le), Right(re)) => Right(Cons(le, re))
        case _ => throw new RuntimeException("Type error")
      }
      case Var(x) => this.map(x)
    }

    def doCommand(cmd: Command): Store = cmd match {
      case Skip() => this
      case Assign(name, exp) =>
        new Store(this.map.updated((name, this.evalExp(exp))))
      case Compose(cmd1, cmd2) => this.doCommand(cmd1).doCommand(cmd2)
      case While(exp, body) =>
        if (this.evalExp(exp) == Atom())
          this
        else
          this.doCommand(Compose(body, cmd))
      case If(exp, cmd1, cmd2) =>
        if (this.evalExp(exp) == Atom())
          this.doCommand(cmd2)
        else
          this.doCommand(cmd1)
    }
  }
}

package Syntax {

  object Syntax {

    type Name = String
    type Prog = Map[Name, Statement]

    /** The expression language is the language of binary trees.
    */
    abstract class Expression
    case class Variable(name: Name) extends Expression
    case class Atom() extends Expression
    case class Cons(hd: Expression, tl: Expression) extends Expression
    case class Head(exp: Expression) extends Expression
    case class Tail(exp: Expression) extends Expression

    /** A while language with very simple concurrency. Each 'node' (a named
    *  instruction) can send messages to other nodes while they execute.
    */
    abstract class Statement
    case class Skip() extends Statement
    case class Input(name: Name, node: Name) extends Statement
    case class Output(exp: Expression, node: Name) extends Statement
    case class While(exp: Expression, stmts: List[Statement]) extends Statement
    case class Assign(name: Name, exp: Expression) extends Statement
  }
}

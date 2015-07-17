package Syntax {
  object Syntax {

    type Name = String
    type Node = (Name, Command)
    type NodeMap = Map[Name, Command]
    type Program = (ChannelDeclarationMap, NodeMap)

    abstract class ChannelDeclaration
    case class Direct(name: Name) extends ChannelDeclaration
    case class Indirect(local: Name, global: Name) extends ChannelDeclaration

    abstract class Tree
    case class Atom() extends Tree
    case class Cons(lhs: Tree, rhs: Tree) extends Tree

    abstract class Expression
    case class Var(name: Name) extends Expression
    case class AtomLit() extends Expression
    case class ConsLit(lhs: Expression, rhs: Expression) extends Expression
    case class Head(exp: Expression) extends Expression
    case class Tail(exp: Expression) extends Expression

    abstract class Command
    case class Skip() extends Command
    case class Assign(name: Name, exp: Expression) extends Command
    case class Compose(cmd1: Command, cmd2: Command) extends Command
    case class While(exp: Expression, cmd: Command) extends Command
    case class If(exp: Expression, cmd1: Command, cmd2: Command) extends Command
    case class NewChannel(name: Name) extends Command
    case class Send(exp: Expression, chan: Name) extends Command
    case class Receive(chan: Name, name: Name) extends Command
  }
}

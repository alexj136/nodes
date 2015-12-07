package test

import syntax._
import interpreter.turner.runWithTurnerMachine
import org.scalacheck._

object InterpreterProperties extends Properties("Proc") {

  val names: Map[Name, String] = Map (
    ( new Name(0), "a" ) ,
    ( new Name(1), "b" ) ,
    ( new Name(2), "c" ) ,
    ( new Name(3), "d" ) ,
    ( new Name(4), "e" ) ,
    ( new Name(5), "f" ) )
    
  val next: Name = new Name(6)

  property("sumall") = {

    /* proc =
     *
     *  send a : x . end |
     *  send a : y . end |
     *  send a : z . end |
     *  receive a : b . receive a : c . receive a : d . (
     *    send a : b + c + d . end |
     *    receive a : e . send a : e . end
     *  )
     *
     * should evaluate to =
     *
     *  send a : (x+y+z) . end
     */
    val proc: Function3[Int, Int, Int, Proc] = (x: Int, y: Int, z: Int) =>
      Proc.fromList(List(
        Send(ChanLiteral(new Name(0)), IntLiteral(x), End),
        Send(ChanLiteral(new Name(0)), IntLiteral(y), End),
        Send(ChanLiteral(new Name(0)), IntLiteral(z), End),
        Receive(false, ChanLiteral(new Name(0)), new Name(1),
          Receive(false, ChanLiteral(new Name(0)), new Name(2),
          Receive(false, ChanLiteral(new Name(0)), new Name(3),
          Send(ChanLiteral(new Name(4)), BinExp(Add, BinExp(Add, Variable(new
            Name(1)), Variable(new Name(2))), Variable(new Name(3))), End)))),
        Receive(false, ChanLiteral(new Name(4)), new Name(5),
          Send(ChanLiteral(new Name(0)), Variable(new Name(5)), End))))
    Prop.forAll { ( x: Int, y: Int, z: Int ) => {
      val (procPost, namesPost, nextPost): (Proc, Map[Name, String], Name) =
        runWithTurnerMachine(proc(x, y, z), names, next)
      procPost.struct(namesPost,
        Send(ChanLiteral(new Name(0)), IntLiteral(x+y+z), End), namesPost)
    }}
  }
}

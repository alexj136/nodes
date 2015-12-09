package test

import syntax._
import interpreter.turner.runWithTurnerMachine
import org.scalacheck._

object TurnerMachineProperties extends Properties("Proc") {

  val names: Map[Name, String] = Map (
    ( Name(0), "a" ) ,
    ( Name(1), "b" ) ,
    ( Name(2), "c" ) ,
    ( Name(3), "d" ) ,
    ( Name(4), "e" ) ,
    ( Name(5), "f" ) )
    
  val next: Name = Name(6)

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
        Send(ChanLiteral(Name(0)), IntLiteral(x), End),
        Send(ChanLiteral(Name(0)), IntLiteral(y), End),
        Send(ChanLiteral(Name(0)), IntLiteral(z), End),
        Receive(false, ChanLiteral(Name(0)), Name(1),
          Receive(false, ChanLiteral(Name(0)), Name(2),
          Receive(false, ChanLiteral(Name(0)), Name(3),
          Send(ChanLiteral(Name(4)), BinExp(Add, BinExp(Add, Variable(Name(1)),
            Variable(Name(2))), Variable(Name(3))), End)))),
        Receive(false, ChanLiteral(Name(4)), Name(5),
          Send(ChanLiteral(Name(0)), Variable(Name(5)), End))))
    Prop.forAll { ( x: Int, y: Int, z: Int ) => {
      val (procPost, namesPost, nextPost): (Proc, Map[Name, String], Name) =
        runWithTurnerMachine(proc(x, y, z), names, next)
      procPost.struct(namesPost,
        Send(ChanLiteral(Name(0)), IntLiteral(x+y+z), End), namesPost)
    }}
  }
}

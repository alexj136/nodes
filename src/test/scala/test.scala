package test

import syntax._
import interpreter.turner.runWithTurnerMachine
import org.scalacheck._

object TurnerMachineProperties extends Properties("Proc") {

  val names: Map[Name, String] = (((0 to 51) map (n => Name(n)))
    .zip(((('a' to 'z') ++ ('A' to 'Z')) map (s => s.toString)))).toMap
    
  val next: Name = Name(52)

  property("addThreeNumbers") = {

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
      val procPost: Proc = runWithTurnerMachine(proc(x, y, z), names, next)._1
      Proc.fromList(procPost.listify.filter({ case x => x != End })).alphaEquiv(
        Proc.fromList(Send(ChanLiteral(Name(0)), IntLiteral(x + y + z), End)
          .listify.filter({ case x => x != End }))).nonEmpty
    }}
  }
}

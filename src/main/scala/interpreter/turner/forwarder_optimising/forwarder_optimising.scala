package interpreter.turner.forwarder_optimising

import syntax._
import interpreter._
import interpreter.turner._

class ForwarderOptimisingTMState(
    run:   List[Proc], 
    wait:  Map[Name, List[Proc]],
    names: Map[Name, String],
    next:  Name) extends TurnerMachineState(run, wait, names, next) {

  override def handleNew(nu: New): Option[MachineState] = nu match {
    case New(r0, _,
          Send(ChanLiteral(a), Pair(x, r1),
          Receive(false, r2, y0, _,
          Send(s, y1, p))))
            if r0 == r1 && r0 == r2 && y0 == y1 =>
              super.handleSend(Send(ChanLiteral(a), Pair(x, s), p))
    case New(r0, _,
          Send(ChanLiteral(a), Pair(r1, x),
          Receive(false, r2, y0, _,
          Send(s, y1, p))))
            if r0 == r1 && r0 == r2 && y0 == y1 =>
              super.handleSend(Send(ChanLiteral(a), Pair(s, x), p))
    case _ => super.handleNew(nu)
  }
}

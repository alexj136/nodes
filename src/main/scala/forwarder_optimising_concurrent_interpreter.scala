package forwarder_optimising_concurrent_interpreter

import akka.actor._
import syntax._
import interpreter_common._
import interpreter_common.Functions._
import concurrent_interpreter._

class FwdOptProcManager(
    launcher: Launcher,
    _nextName: Name)
  extends ProcManager(launcher, _nextName) {

  override def makeRunner(chanMap: Map[Name, ActorRef], p: Proc): Unit = {
    this.nextName = this.nextName.next
    val newRunner: ActorRef = context.actorOf(Props(classOf[FwdOptProcRunner],
      chanMap, p, self), s"FwdOptProcRunner${this.nextName.id}")
    this.liveActors = this.liveActors + newRunner
    newRunner ! ProcGo
  }
}

class FwdOptProcRunner(
    _chanMap: Map[Name, ActorRef],
    _proc: Proc,
    procManager: ActorRef)
  extends ProcRunner(_chanMap, _proc, procManager) {

  private var optimisationAttempted: Boolean = false

  override def handleServer(chExp: Exp, bind: Name, p: Proc): Unit = {
    if (this.optimisationAttempted) super.handleServer(chExp, bind, p)
    else {
      this.proc = fwdOptRewrite(this.proc)
      this.optimisationAttempted = true
      self ! ProcGo
    }
  }
}

object fwdOptRewrite extends Function1[Proc, Proc] {

  override def apply(p: Proc): Proc = p match {
    case Send(chExp, msg, p) => Send(chExp, msg, fwdOptRewrite(p))

    case Receive(repl, chExp, bind, p) =>
      Receive(repl, chExp, bind, fwdOptRewrite(p))

    case LetIn(bind, exp, p) => LetIn(bind, exp, fwdOptRewrite(p))

    case IfThenElse(exp, p, q) =>
      IfThenElse(exp, fwdOptRewrite(p), fwdOptRewrite(q))

    case Parallel(p, q) => Parallel(fwdOptRewrite(p), fwdOptRewrite(q))

    // need a function to determine if msg0 contains bind0 as part of the
    //    message
    // if msg0 contains bind0
    // if chExp1 is bind0
    // if msg1 is bind1
    /*case New(bind0,
          Send(chExp0, msg0,
          Receive(false, chExp1, bind1,
          Send(chExp2, msg1, p))))
             => ???*/

    case New(bind, p) => New(bind, fwdOptRewrite(p))

    case End => End
  }
}

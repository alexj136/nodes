package interpreter.concurrent.forwarder_optimising.runtime_conditional

import akka.actor._
import syntax._
import interpreter._
import interpreter.concurrent._

class RunTCondFwdOptProcRunner(
    _parent: Option[ActorRef],
    _chanMap: Map[Name, ActorRef],
    _proc: Proc,
    procManager: ActorRef)
  extends ProcRunner(_parent, _chanMap, _proc, procManager) {

  def isServer: Boolean = this.proc match {
    case Receive(true, _, _, _) => true
    case _                      => false
  }

  override def handleServer(chExp: Exp, bind: Name, p: Proc): Unit = ???

  override def handleMetaMessageReceived(metaInfo: MetaInfo): Unit =
    metaInfo match {
      case UseAlternateChannel(oldChName, newChRef) if this.isServer =>
        ???
      case NotifyParent(moreInfo) => this.parent map (_ ! moreInfo)
      case _ => super.handleMetaMessageReceived(metaInfo)
    }
}

object serverRewrite extends Function1[Proc, Option[Proc]] {

  override def apply(p: Proc): Option[Proc] = p match {
    case Send(chExp, msg, p) => serverRewrite(p) map (p => Send(chExp, msg, p))

    case Receive(repl, chExp, bind, p) =>
      serverRewrite(p) map (p => Receive(repl, chExp, bind, p))

    case LetIn(bind, exp, p) => serverRewrite(p) map (p => LetIn(bind, exp, p))

    case IfThenElse(exp, p, q) => for {
      newP <- serverRewrite(p)
      newQ <- serverRewrite(q)
    } yield IfThenElse(exp, newP, newQ)

    case Parallel(p, q) => for {
      newP <- serverRewrite(p)
      newQ <- serverRewrite(q)
    } yield Parallel(newP, newQ)

    case New(nrch0,
         Send(chExp0, msg0,
         Receive(false, Variable(nrch1), reply0,
         IfThenElse(condExp,
           Send(orch0, Variable(reply1), End),
           Send(orch1, Variable(reply2), End)))))
             if ((msg0 contains nrch0)
             && (nrch0 == nrch1)
             && (orch0 == orch1)
             && (reply0 == reply1)
             && (reply0 == reply2))
               => (Some(???): Option[Proc])

    case New(bind, p) => serverRewrite(p) map (p => New(bind, p))

    case End => (None: Option[Proc])
  }
}

// Signals to a server caller that a new server can be used, so substitute the
// channel name of the old server with a new one.
case class UseAlternateChannel(
    oldChName: Name,
    newChRef: ActorRef)
  extends MetaInfo

// Tells a process to pass the contained MetaInfo on to its parent process.
case class NotifyParent(metaInfo: MetaInfo) extends MetaInfo

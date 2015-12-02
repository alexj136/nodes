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

  override def handleMetaMessageReceived(
      metaInfo: MetaInfo)
  : Unit = metaInfo match {

    case UseAlternateChannel(
        oldChName,
        newChName,
        newChRef) if this.isServer => {

      this.chanMap = this.chanMap updated (newChName, newChRef)
      this.proc = ???/*this.substituteProc(this.proc, oldChName,
        ChanLiteral(newChName))*/
    }

    case NotifyParent(moreInfo) => this.parent map (_ ! moreInfo)

    case _ => super.handleMetaMessageReceived(metaInfo)
  }
}

object forwarderRewrite extends Function1[Proc, Option[Proc]] {

  override def apply(p: Proc): Option[Proc] = p match {
    case Send(chExp, msg, p) =>
      forwarderRewrite(p) map (p => Send(chExp, msg, p))

    case Receive(repl, chExp, bind, p) =>
      forwarderRewrite(p) map (p => Receive(repl, chExp, bind, p))

    case LetIn(bind, exp, p) =>
      forwarderRewrite(p) map (p => LetIn(bind, exp, p))

    case IfThenElse(exp, p, q) => for {
      newP <- forwarderRewrite(p)
      newQ <- forwarderRewrite(q)
    } yield IfThenElse(exp, newP, newQ)

    case Parallel(p, q) => for {
      newP <- forwarderRewrite(p)
      newQ <- forwarderRewrite(q)
    } yield Parallel(newP, newQ)

    /* Here we match conditional forwards. These processes create a new channel
     * and send a computation request, expecting a reply on the new channel.
     * Hence the outgoing message msg0 must be an expression containing the new
     * channel nrch0.
     */
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

    case New(bind, p) => forwarderRewrite(p) map (p => New(bind, p))

    case End => (None: Option[Proc])
  }
}

// Signals to a server caller that a new server can be used, so substitute the
// channel name of the old server with a new one.
case class UseAlternateChannel(
    oldChName: Name,
    newChName: Name,
    newChRef: ActorRef)
  extends MetaInfo

// Tells a process to pass the contained MetaInfo on to its parent process.
case class NotifyParent(metaInfo: MetaInfo) extends MetaInfo

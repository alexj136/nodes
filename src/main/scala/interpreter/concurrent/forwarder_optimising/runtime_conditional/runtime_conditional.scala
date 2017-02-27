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
  import Transformations._

  def isServer: Boolean = this.proc match {
    case Receive(true, _, _, _, _) => true
    case _                         => false
  }

  override def handleMetaMessageReceived(
      metaInfo: MetaInfo)
    : Unit = metaInfo match {

    case LogChildPath(conditionExp: Exp) if this.isServer => ???

    case UseAlternateChannel(
        oldChName,
        newChName,
        newChRef) if this.isServer => {

      this.chanMap = this.chanMap updated (newChName, newChRef)
      this.proc = renameChannelInProc(this.proc, oldChName, newChName)
    }

    case NotifyParent(moreInfo) => this.parent map (_ ! moreInfo)

    case _ => super.handleMetaMessageReceived(metaInfo)
  }
}

object Transformations {

  def forwarderRewrite(p: Proc): Option[Proc] = p match {
    case Send(chExp, msg, p) =>
      forwarderRewrite(p) map (p => Send(chExp, msg, p))

    case Receive(repl, chExp, bind, ty, p) =>
      forwarderRewrite(p) map (p => Receive(repl, chExp, bind, ty, p))

    case LetIn(bind, ty, exp, p) =>
      forwarderRewrite(p) map (p => LetIn(bind, ty, exp, p))

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
    case New(nrch0, _,
         Send(chExp0, msg0,
         Receive(false, Variable(nrch1), reply0, _,
         IfThenElse(condExp,
           Send(orch0, Variable(reply1), End),
           Send(orch1, Variable(reply2), End)))))
             if ((msg0 contains nrch0)
             && (nrch0 == nrch1)
             && (orch0 == orch1)
             && (reply0 == reply1)
             && (reply0 == reply2))
               => (Some(???): Option[Proc])

    case New(bind, ty, p) => forwarderRewrite(p) map (p => New(bind, ty, p))

    case End => (None: Option[Proc])
  }

  def renameChannelInProc(proc: Proc, from: Name, to: Name): Proc = {
    val renameP: Function1[Proc, Proc] = p => renameChannelInProc(p, from, to)
    val renameE: Function1[Exp , Exp ] = e => renameChannelInExp (e, from, to)
    proc match {
      case Send       ( c , e , p         ) =>
        Send(renameE(c), renameE(e), renameP(p))
      case Receive    ( s , c , n , t , p ) =>
        Receive(s, renameE(c), n, t, if (n != from) renameP(p) else p)
      case LetIn      ( n , t , e , p     ) =>
        LetIn(n, t, renameE(e), if (n != from) renameP(p) else p)
      case IfThenElse ( e , p , q         ) =>
        IfThenElse(renameE(e), renameP(p), renameP(q))
      case Parallel   ( p , q             ) =>
        Parallel(renameP(p), renameP(q))
      case New        ( n , t , p         ) =>
        New(n, t, if (n != from) renameP(p) else p)
      case End                              => End
    }
  }

  def renameChannelInExp(exp: Exp, from: Name, to: Name): Exp = {
    val renameE: Exp => Exp = e => renameChannelInExp(e, from, to)
    exp match {
      case Variable    ( n         ) => exp
      case IntLiteral  ( x         ) => exp
      case BoolLiteral ( x         ) => exp
      case ChanLiteral ( c         ) => if (c == from) ChanLiteral(to) else exp
      case KharLiteral ( c         ) => exp
      case Pair        ( l , r     ) => Pair(renameE(l), renameE(r))
      case UnExp       ( t , e     ) => UnExp(t, renameE(e))
      case BinExp      ( t , l , r ) => BinExp(t, renameE(l), renameE(r))
      case ListExp     ( es        ) => ListExp(es map renameE)
    }
  }
}

// Records info about the path through conditionals required to reach the hot
// sending code
case class LogChildPath(conditionExp: Exp) extends MetaInfo

// Signals to a server caller that a new server can be used, so substitute the
// channel name of the old server with a new one.
case class UseAlternateChannel(
    oldChName: Name,
    newChName: Name,
    newChRef: ActorRef)
  extends MetaInfo

// Tells a process to pass the contained MetaInfo on to its parent process.
case class NotifyParent(metaInfo: MetaInfo) extends MetaInfo

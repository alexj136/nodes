package concurrent_interpreter

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor._
import akka.pattern.{Patterns, ask}
import syntax._
import interpreter_common._
import interpreter_common.Functions._

class Launcher(p: Proc, onCompletion: Function1[Proc, Unit]) {

  val (system: ActorSystem, procManager: ActorRef) = {

    val sys: ActorSystem = ActorSystem("Launcher")
    sys.registerOnTermination(new Runnable {
      def run: Unit = onCompletion(result.get)
    })
    val procManager: ActorRef =
      sys.actorOf(Props(classOf[ProcManager], this), "ProcManager")
    val initChanMap: Map[Name, ActorRef] = (p.free map { case n =>
      (n, sys.actorOf(Props(classOf[Channel], procManager),
        s"ChannelI${n.id}")) }).toMap
    procManager ! SetLiveActors(initChanMap.values.toSet)
    procManager ! MakeRunner(initChanMap, p)
    sys.scheduler.schedule(3.seconds, 3.seconds, procManager,
      CheckFinished)(sys.dispatcher)

    (sys, procManager)
  }

  var result: Option[Proc] = None
}

// Serves as parent actor for other actors. Keeps track of channels and runners.
// Can be queried for deadlock detection.
class ProcManager(launcher: Launcher) extends Actor {

  var liveActors: Set[ActorRef] = Set.empty
  var result: List[Proc] = Nil
  var sendSinceTimerReset: Boolean = false
  var reportTo: Option[ActorRef] = None

  def receive: Receive = setLiveActors

  def setLiveActors: Receive = {
    case SetLiveActors(set) => {
      this.liveActors = set
      context.become(mainReceive)
      this.reportTo = Some(sender)
    }
  }

  def mainReceive: Receive = {
    case SendOccurred => sendSinceTimerReset = true
    case CheckFinished => {
      if (sendSinceTimerReset) sendSinceTimerReset = false
      else this.liveActors map { case p => p ! ForceReportStop }
    }
    case ReportStop(p) => {
      this.result = this.result ++ p.toList
      this.liveActors = this.liveActors - sender
      Await.result(Patterns.gracefulStop(sender, 10.seconds), Duration.Inf)
      if (this.liveActors.isEmpty) {
        launcher.result = Some(Proc fromList this.result)
        context.system.shutdown
      }
    }
    case MakeChannel => {
      val newChannel: ActorRef = context.actorOf(Props(classOf[Channel],
        self), s"Channel${this.liveActors.size}")
      sender ! MakeChannelResponse(newChannel)
      this.liveActors = this.liveActors + newChannel
    }
    case MakeRunner(chanMap, p) => {
      val newRunner: ActorRef = context.actorOf(Props(classOf[ProcRunner],
        chanMap, p, self), s"ProcRunner${this.liveActors.size}")
      newRunner ! ProcGo
      this.liveActors = this.liveActors + newRunner
    }
  }
}

abstract class AbstractImplActor(val procManager: ActorRef) extends Actor {

  def reportValue: Option[Proc] = None

  def forceReportStop: Receive = {
    case ForceReportStop => {
      this.procManager ! ReportStop(this.reportValue)
    }
  }
}

// Runs a process
class ProcRunner(
  var chanMap: Map[Name, ActorRef],
  var varMap: Map[Name, EvalExp],
  var proc: Proc,
  procManager: ActorRef) extends AbstractImplActor(procManager) {

  override def reportValue: Option[Proc] = Some(this.proc)

  def receive = ({
    case ProcGo => this.proc match {
      case Send(chExp, msg, p) => {
        val evalChExp: EvalExp = evalExp(chExp)
        val evalMsg: EvalExp = evalExp(msg)
        this.chanMap(evalChExp.channelName) ! MsgSenderToChan(evalMsg)
        this.proc = p
        context.become(({ case MsgConfirmToSender => {
          context.unbecome()
          self ! ProcGo
        }}: Receive) orElse forceReportStop)
      }
      case Receive(repl, chExp, bind, p) => {
        val evalChExp: EvalExp = evalExp(chExp)
        this.chanMap(evalChExp.channelName) ! MsgRequestFromReceiver
        if(repl) {
          this.procManager ! MakeRunner(this.chanMap, this.proc)
        }
        this.proc = p
        context.become(({ case MsgChanToReceiver(evalMsg) => {
          this.varMap = this.varMap.updated(bind, evalMsg)
          context.unbecome()
          self ! ProcGo
        }}: Receive) orElse forceReportStop)
      }
      case LetIn(bind, exp, p) => ???
      case IfThenElse(exp, p, q) => ???
      case Parallel(p, q) => {
        this.procManager ! MakeRunner(this.chanMap, q)
        this.proc = p
        self ! ProcGo
      }
      case New(name, p) => {
        this.procManager ! MakeChannel
        context.become(({ case MakeChannelResponse(channel) => {
          this.chanMap = this.chanMap.updated(name, channel)
          this.proc = p
          context.unbecome()
          self ! ProcGo
        }}: Receive) orElse forceReportStop)
      }
      case End             => this.procManager ! ReportStop(None)
    }
  }: Receive) orElse forceReportStop
}

// Implements the behaviour of channels in pi calculus
class Channel(procManager: ActorRef) extends AbstractImplActor(procManager) {

  def deliver(sndr: ActorRef, rcvr: ActorRef, evalMsg: EvalExp): Unit = {
        procManager ! SendOccurred
        rcvr ! MsgChanToReceiver(evalMsg)
        sndr ! MsgConfirmToSender
  }

  def receive = ({
    // If the receiver request comes before the sender delivery
    case MsgRequestFromReceiver => {
      val msgReceiver: ActorRef = sender
      context.become(({ case MsgSenderToChan(evalMsg) => {
        val msgSender: ActorRef = sender
        this.deliver(msgSender, msgReceiver, evalMsg)
        context.unbecome()
      }}: Receive) orElse forceReportStop)
    }
    // If the sender delivery comes before the receiver request
    case MsgSenderToChan(evalMsg) => {
      val msgSender: ActorRef = sender
      context.become(({ case MsgRequestFromReceiver => {
        val msgReceiver: ActorRef = sender
        this.deliver(msgSender, msgReceiver, evalMsg)
        context.unbecome()
      }}: Receive) orElse forceReportStop)
    }
  }: Receive) orElse forceReportStop
}

// Top class for messages sent in this implementation
sealed abstract class ImplMessage

// Queries sent by ProcRunners to Channels
sealed abstract class ChanQuery extends ImplMessage

// Precursor to a MsgConfirmToSender ChanQueryResponse
case class  MsgSenderToChan(msg: EvalExp) extends ChanQuery

// Precursor to a MsgChanToReceiver ChanQueryResponse
case object MsgRequestFromReceiver             extends ChanQuery

// Responses sent by Channels to ProcRunners
sealed abstract class ChanQueryResponse extends ImplMessage

// Complements a MsgSenderToChan ChanQuery
case object MsgConfirmToSender                   extends ChanQueryResponse

// Complements a MsgRequestFromReceiver ChanQuery
case class  MsgChanToReceiver(msg: EvalExp) extends ChanQueryResponse

// Used to tell the procManager to create a new process or channel
sealed abstract class CreationRequest extends ImplMessage

// Requests a new channel
case object MakeChannel extends CreationRequest

// Requests a new process
case class  MakeRunner(chanMap: Map[Name, ActorRef], p: Proc)
  extends CreationRequest

// Signals that a channel has been created to a process that requested a new one
case class MakeChannelResponse(channel: ActorRef) extends ImplMessage

// Signalling object sent to ProcRunners to tell them to do a computation step
case object ProcGo extends ImplMessage

// Tells AbstractImplActors to report their status to their procManager which
// will then stop them
case object ForceReportStop extends ImplMessage

// Used by AbstractImplActors to tell their procManager their status before
// being stopped
case class ReportStop(op: Option[Proc]) extends ImplMessage

// Used by the Launcher to give the references to the initial channels to the
// procManager
case class SetLiveActors(actorset: Set[ActorRef]) extends ImplMessage

// Tells the procManager to test if the system has finished
case object CheckFinished extends ImplMessage

// Tells the procManager that a send has occurred since the last tick
case object SendOccurred extends ImplMessage

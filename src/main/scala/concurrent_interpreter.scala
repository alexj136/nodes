package concurrent_interpreter

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor._
import akka.pattern.{Patterns, ask}
import syntax._
import interpreter._

/**
 * This module gives an interpreted concurrent implementation of the nodes
 * language using the akka library. There are 4 major classes used in this
 * implementation. These are:
 *    - Launcher: responsible for initiating the interpreter and interfacing
 *      with the outside. One instance per invocation.
 *    - ProcManager: a one-per-invocation actor that manages and keeps track of
 *      all other actors in the system. Creates new ProcRunners and Channels
 *      when required, when requested to do so by a ProcRunner, either because
 *      a new channel has been created, or because a fork (parallel composition
 *      under an input or output) has been executed.
 *    - ProcRunner: One per source-level process. Responsible for 'executing'
 *      the process it contains, communicating with other ProcRunners via
 *      Channels to exchange messages. A ProcRunner keeps a mapping between
 *      ChanLiteral Names and the ActorRefs that represent those channels at
 *      runtime. Every time a ProcRunner sends a message, it must also send any
 *      mappings required for any ChanLiterals present in the sent message.
 *    - Channel: Represents a pi calculus channel. At the implementation level,
 *      it is a sort of latch - it waits for a sender and a receiver to tell it
 *      that they each want to send and receive a message respectively, on this
 *      channel. When this occurs, the messgage (and required channel mappings)
 *      are taken from the sender and given to the receiver.
 */

class Launcher(
    p: Proc,
    printName: Name,
    nextName: Name,
    names: Map[Name, String],
    onCompletion: Function1[Proc, Unit],
    procRunnerClass: Class[_ <: ProcRunner] = classOf[ProcRunner]) {

  var result: Option[Proc] = None

  val (system: ActorSystem, procManager: ActorRef) = {

    val sys: ActorSystem = ActorSystem("Launcher")

    sys.registerOnTermination(new Runnable {
      def run: Unit = onCompletion(result.get)
    })

    val procManager: ActorRef = sys.actorOf(
      Props(classOf[ProcManager], this, nextName, procRunnerClass),
      "ProcManager")

    val initChanMap: Map[Name, ActorRef] = (p.chanLiterals map {

      case n if (n == printName) => (n, sys.actorOf(
        Props(classOf[PrintingChannel], names, procManager), s"LIT$$print"))

      case n if (n != printName) => (n, sys.actorOf(
        Props(classOf[Channel], procManager), s"LIT${names(n)}"))

    }).toMap

    procManager ! SetLiveActors(initChanMap.values.toSet)

    procManager ! MakeRunner(None, initChanMap, p)

    sys.scheduler.schedule(3.seconds, 3.seconds, procManager,
      CheckFinished)(sys.dispatcher)

    (sys, procManager)
  }
}

// Serves as parent actor for other actors. Keeps track of channels and runners.
// Can be queried for deadlock detection.
class ProcManager(
    launcher: Launcher,
    var nextName: Name,
    procRunnerClass: Class[_ <: ProcRunner])
  extends Actor {
  
  var liveActors: Set[ActorRef] = Set.empty
  var result: List[Proc] = Nil
  var sendSinceTimerReset: Boolean = false

  def receive: Receive = setLiveActors

  def makeChannel: (Name, ActorRef) = {
    this.nextName = this.nextName.next
    val newChannel: ActorRef = context.actorOf(Props(classOf[Channel],
      self), s"NEW${this.nextName.id}")
    this.liveActors = this.liveActors + newChannel
    (this.nextName, newChannel)
  }

  def makeRunner(
      parent: Option[ActorRef],
      chanMap: Map[Name, ActorRef],
      p: Proc)
  : Unit = {
    this.nextName = this.nextName.next
    val newRunner: ActorRef = context.actorOf(Props(procRunnerClass, parent,
      chanMap, p, self), s"${procRunnerClass.getName}${this.nextName.id}")
    this.liveActors = this.liveActors + newRunner
    newRunner ! ProcGo
  }

  def setLiveActors: Receive = {
    case SetLiveActors(set) => {
      this.liveActors = set
      context.become(mainReceive)
    }
  }

  def mainReceive: Receive = {
    case SendOccurred => {
      sendSinceTimerReset = true
    }
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
      val (name, chan): (Name, ActorRef) = this.makeChannel
      sender ! MakeChannelResponse(name, chan)
    }
    case MakeRunner(parent, chanMap, p) => this.makeRunner(parent, chanMap, p)
  }
}

abstract class AbstractImplActor(val procManager: ActorRef) extends Actor {

  def reportValue: Option[Proc] = None

  def defaultBehaviours: Receive = {
    case ForceReportStop => {
      this.procManager ! ReportStop(this.reportValue)
    }
  }
}

// Runs a process
class ProcRunner(
    val parent: Option[ActorRef],
    var chanMap: Map[Name, ActorRef],
    var proc: Proc,
    procManager: ActorRef)
  extends AbstractImplActor(procManager) {

  override def reportValue: Option[Proc] = Some(this.proc)

  override def defaultBehaviours: Receive = super.defaultBehaviours orElse {
    case metaInfo: MetaInfo => this.handleMetaMessageReceived(metaInfo)
  }

  def handleSend(chExp: Exp, msg: Exp, p: Proc): Unit = {
    val evalChExp: EvalExp = EvalExp from chExp
    val evalMsg: EvalExp = EvalExp from msg
    this.chanMap(evalChExp.channelName) ! MsgSenderToChan(evalMsg,
      this.computeMetaMessageToSend,
      this.chanMap.filterKeys(evalMsg.channelNames.contains(_)))
    context.become(({ case MsgConfirmToSender => {
      this.proc = p
      context.unbecome()
      self ! ProcGo
    }}: Receive) orElse defaultBehaviours)
  }

  def handleReceive(chExp: Exp, bind: Name, p: Proc): Unit = {
    val evalChExp: EvalExp = EvalExp from chExp
    this.chanMap(evalChExp.channelName) ! MsgRequestFromReceiver
    context.become(({
      case MsgChanToReceiver(evalMsg, metaInfo, newMappings) => {
        val newProc: Proc = substituteProc(p, bind, evalMsg)
        val newChanMap: Map[Name, ActorRef] = this.chanMap ++ newMappings
        this.proc = newProc
        this.chanMap = newChanMap
        this.handleMetaMessageReceived(metaInfo)
        context.unbecome()
        self ! ProcGo
      }
    }: Receive) orElse defaultBehaviours)
  }

  def handleServer(chExp: Exp, bind: Name, p: Proc): Unit = {
    val evalChExp: EvalExp = EvalExp from chExp
    this.chanMap(evalChExp.channelName) ! MsgRequestFromReceiver
    context.become(({
      case MsgChanToReceiver(evalMsg, metaInfo, newMappings) => {
        val newProc: Proc = substituteProc(p, bind, evalMsg)
        val newChanMap: Map[Name, ActorRef] = this.chanMap ++ newMappings
        this.procManager ! MakeRunner(Some(self), newChanMap, newProc)
        this.handleMetaMessageReceived(metaInfo)
        context.unbecome()
        self ! ProcGo
      }
    }: Receive) orElse defaultBehaviours)
  }

  def handleLetIn(bind: Name, exp: Exp, p: Proc): Unit = {
    this.proc = substituteProc(p, bind, EvalExp from exp)
    self ! ProcGo
  }

  def handleIfThenElse(exp: Exp, p: Proc, q: Proc): Unit = {
    EvalExp from exp match {
      case EEBool(true ) => this.proc = p
      case EEBool(false) => this.proc = q
      case _ => ???
    }
    self ! ProcGo
  }

  def handleParallel(p: Proc, q: Proc): Unit = {
    this.procManager ! MakeRunner(this.parent, this.chanMap, q)
    this.proc = p
    self ! ProcGo
  }

  def handleNew(name: Name, p: Proc): Unit = {
    this.procManager ! MakeChannel
    context.become(({ case MakeChannelResponse(id, channel) => {
      this.chanMap = this.chanMap.updated(id, channel)
      this.proc = substituteProc(p, name, EEChan(id))
      context.unbecome()
      self ! ProcGo
    }}: Receive) orElse defaultBehaviours)
  }

  def handleCurrentProcess: Unit = this.proc match {
    case    Send       ( chExp , msg   , p        ) =>
      handleSend       ( chExp , msg   , p        )
    case    Receive    ( false , chExp , bind , p ) =>
      handleReceive    (         chExp , bind , p )
    case    Receive    ( true  , chExp , bind , p ) =>
      handleServer     (         chExp , bind , p )
    case    LetIn      ( bind  , exp   , p        ) =>
      handleLetIn      ( bind  , exp   , p        )
    case    IfThenElse ( exp   , p     , q        ) =>
      handleIfThenElse ( exp   , p     , q        )
    case    Parallel   ( p     , q                ) =>
      handleParallel   ( p     , q                )
    case    New        ( name  , p                ) =>
      handleNew        ( name  , p                )
    case End => this.procManager ! ReportStop(None)
  }

  def receive = ({
    case ProcGo => this.handleCurrentProcess
  }: Receive) orElse defaultBehaviours

  def handleMetaMessageReceived(metaInfo: MetaInfo): Unit = metaInfo match {
    case _ => Unit
  }

  def computeMetaMessageToSend: MetaInfo = NoInfo
}

// Implements the behaviour of channels in pi calculus
class Channel(procManager: ActorRef) extends AbstractImplActor(procManager) {

  def deliver(
      sndr: ActorRef,
      rcvr: ActorRef,
      evalMsg: EvalExp,
      metaInfo: MetaInfo,
      newMappings: Map[Name, ActorRef])
    : Unit = {

    procManager ! SendOccurred
    rcvr ! MsgChanToReceiver(evalMsg, metaInfo, newMappings)
    sndr ! MsgConfirmToSender
  }

  def receive = ({
    // If the receiver request comes before the sender delivery
    case MsgRequestFromReceiver => {
      val msgReceiver: ActorRef = sender
      context.become(({
        case MsgSenderToChan(evalMsg, metaInfo, newMappings) => {
          val msgSender: ActorRef = sender
          this.deliver(msgSender, msgReceiver, evalMsg, metaInfo, newMappings)
          context.unbecome()
        }
      }: Receive) orElse defaultBehaviours)
    }
    // If the sender delivery comes before the receiver request
    case MsgSenderToChan(evalMsg, metaInfo, newMappings) => {
      val msgSender: ActorRef = sender
      context.become(({
        case MsgRequestFromReceiver => {
          val msgReceiver: ActorRef = sender
          this.deliver(msgSender, msgReceiver, evalMsg, metaInfo, newMappings)
          context.unbecome()
        }
      }: Receive) orElse defaultBehaviours)
    }
  }: Receive) orElse defaultBehaviours
}

class PrintingChannel(
    names: Map[Name, String],
    procManager: ActorRef)
  extends AbstractImplActor(procManager) {

  def receive: Receive = ({
    case MsgSenderToChan(evalMsg, _, _) => {
      procManager ! SendOccurred
      println(evalMsg.unEvalExp pstr this.names)
      sender ! MsgConfirmToSender
    }
  }: Receive) orElse defaultBehaviours
}

// Top class for messages sent in this implementation
sealed abstract class ImplMessage

// Queries sent by ProcRunners to Channels
sealed abstract class ChanQuery extends ImplMessage

// Top-level class for meta information
abstract class MetaInfo

// This implementation does not use any meta information, so we define here only
// a single concrete MetaInfo class called NoInfo
case object NoInfo extends MetaInfo

// Precursor to a MsgConfirmToSender ChanQueryResponse
case class  MsgSenderToChan(
    msg: EvalExp,
    metaInfo: MetaInfo,
    chans: Map[Name, ActorRef])
  extends ChanQuery

// Precursor to a MsgChanToReceiver ChanQueryResponse
case object MsgRequestFromReceiver             extends ChanQuery

// Responses sent by Channels to ProcRunners
sealed abstract class ChanQueryResponse extends ImplMessage

// Complements a MsgSenderToChan ChanQuery
case object MsgConfirmToSender                   extends ChanQueryResponse

// Complements a MsgRequestFromReceiver ChanQuery
case class  MsgChanToReceiver(
    msg: EvalExp,
    metaInfo: MetaInfo,
    chans: Map[Name, ActorRef])
  extends ChanQueryResponse

// Used to tell the procManager to create a new process or channel
sealed abstract class CreationRequest extends ImplMessage

// Requests a new channel
case object MakeChannel extends CreationRequest

// Requests a new process
case class  MakeRunner(
    parent: Option[ActorRef],
    chanMap: Map[Name, ActorRef],
    p: Proc)
  extends CreationRequest

// Signals that a channel has been created to a process that requested a new one
case class MakeChannelResponse(
    chanLiteralID: Name,
    channel: ActorRef)
  extends ImplMessage

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

package tracing_interpreter

import syntax._
import interpreter.Evaluator._

object TracingInterpreter {

  sealed abstract class ProcQueue {
    def sends    : Boolean
    def receives : Boolean
    def headSend: (Exp, Proc) = this match {
      case SingletonSendQueue    (e   ) => e
      case MultiSendQueue        (e, _) => e
      case EmptyQueue                   =>
        throw new RuntimeException("took head of empty queue")
      case _                            =>
        throw new RuntimeException("took Receive from Send queue")
    }
    def headReceive: (Option[Int], Name, Proc) = this match {
      case SingletonReceiveQueue (e   ) => e
      case MultiReceiveQueue     (e, _) => e
      case EmptyQueue                   =>
        throw new RuntimeException("took head of empty queue")
      case _                            =>
        throw new RuntimeException("took Send from Receive queue")
    }
    def tail: ProcQueue = this match {
      case SingletonSendQueue    (_   ) => EmptyQueue
      case MultiSendQueue        (_, t) => t
      case SingletonReceiveQueue (_   ) => EmptyQueue
      case MultiReceiveQueue     (_, t) => t
      case EmptyQueue                   =>
        throw new RuntimeException("took tail of empty queue")
    }
    def append(elem: Either[(Exp, Proc), (Option[Int], Name, Proc)]): ProcQueue = this match {
      case SingletonSendQueue    (e      ) =>
        MultiSendQueue   (e, EmptyQueue.append(elem).asInstanceOf[   SendQueue])
      case MultiSendQueue        (e, rest) =>
        MultiSendQueue   (e,       rest.append(elem).asInstanceOf[   SendQueue])
      case SingletonReceiveQueue (e      ) =>
        MultiReceiveQueue(e, EmptyQueue.append(elem).asInstanceOf[ReceiveQueue])
      case MultiReceiveQueue     (e, rest) =>
        MultiReceiveQueue(e,       rest.append(elem).asInstanceOf[ReceiveQueue])
      case EmptyQueue => elem match {
        case Left  (e) => SingletonSendQueue    (e)
        case Right (e) => SingletonReceiveQueue (e)
      }
    }
    def toProcs(ch: Name): List[Proc] = this match {
      case SingletonSendQueue    ((msg, p)             ) =>
        Send(                  ChanLiteral(ch),  msg, p) :: Nil
      case MultiSendQueue        ((msg, p)       , rest) =>
        Send(                  ChanLiteral(ch),  msg, p) :: rest.toProcs(ch)
      case SingletonReceiveQueue ((repl, bind, p)      ) =>
        Receive(!repl.isEmpty, ChanLiteral(ch), bind, p) :: Nil
      case MultiReceiveQueue     ((repl, bind, p), rest) =>
        Receive(!repl.isEmpty, ChanLiteral(ch), bind, p) :: rest.toProcs(ch)
      case EmptyQueue                                    => Nil
    }
  }
  sealed abstract class SendQueue extends ProcQueue {
    override def sends    : Boolean = true
    override def receives : Boolean = false
  }
  case class SingletonSendQueue(elem: (Exp, Proc)) extends SendQueue
  case class MultiSendQueue(elem: (Exp, Proc), rest: SendQueue)
    extends SendQueue
  sealed abstract class ReceiveQueue extends ProcQueue {
    override def sends    : Boolean = false
    override def receives : Boolean = true
  }
  case class SingletonReceiveQueue(elem: (Option[Int], Name, Proc))
    extends ReceiveQueue
  case class MultiReceiveQueue(
    elem: (Option[Int], Name, Proc),
    rest: ReceiveQueue)
    extends ReceiveQueue
  case object EmptyQueue extends ProcQueue {
    override def sends    : Boolean = false
    override def receives : Boolean = false
  }


  class TracingMachineState(
      run: List[Proc],
      wait: Map[Name, ProcQueue],
      names: Map[Name, String],
      next: Name) extends MachineState {

    override def toProc = ???

    def withRun(newRun: List[Proc]): TracingMachineState =
      new TracingMachineState(newRun, this.wait, this.names, this.next)

    def withWait(ch: Name, onCh: ProcQueue): TracingMachineState =
      new TracingMachineState(
        this.run, this.wait.updated(ch, onCh), this.names, this.next)

    def withNext(newNext: Name): TracingMachineState =
      new TracingMachineState(this.run, this.wait, this.names, newNext)

    override def step: Option[MachineState] = this.run match {
      case Nil => None

      case Send(ChanLiteral(ch), msg, p) :: runTail
        if this.names.get(ch) == Some("$print") => {
          println(evalExp(msg).unEvalExp pstr this.names)
          this.withRun(p :: runTail).someOf
        }

      case Send(ChanLiteral(ch), msg, p) :: runTail =>
        if (this.wait(ch).receives) {
          val (copies, bind, q): (Option[Int], Name, Proc) =
            this.wait(ch).headReceive
          val moreRecs: ProcQueue = this.wait(ch).tail
          val qSub: Proc = substituteProc(q, bind, evalExp(msg))
          val waitTail: ProcQueue = if (copies.isEmpty) moreRecs
            else moreRecs append Right(copies map (_ + 1), bind, q)
          this.withRun(p :: (runTail :+ qSub)).withWait(ch, waitTail).someOf
        }
        else this.withRun(runTail)
          .withWait(ch, this.wait(ch) append Left(msg, p))
          .someOf

      case Send(chExp, msg, p) :: runTail => evalExp(chExp) match {
        case EEChan(ch) =>
          this.withRun(Send(ChanLiteral(ch), msg, p) :: runTail).someOf
        case _          => throw FreeVariableError(Send(chExp, msg, p))
      }

      case Receive(repl, ChanLiteral(ch), bind, p) :: runTail =>
        if (this.wait(ch).sends) {
          val (msg, q): (Exp, Proc) = this.wait(ch).headSend
          val moreSends: ProcQueue = this.wait(ch).tail
          val pSub: Proc = substituteProc(p, bind, evalExp(msg))
          val newRun: List[Proc] =
            if (repl)
              Receive(true, ChanLiteral(ch), bind, p) ::
                (runTail :+ pSub :+ q)
            else
              pSub :: (runTail :+ q)
          this.withRun(newRun)
              .withWait(ch, moreSends)
              .someOf
        }
        else this.withRun(runTail)
          .withWait(ch,
            this.wait(ch) append Right(if (repl) Some(0) else None, bind, p))
          .someOf

      case Receive(rep, chExp, bind, p) :: runTail => evalExp(chExp) match {
        case EEChan(ch) =>
          this.withRun(Receive(rep, ChanLiteral(ch), bind, p) :: runTail).someOf
        case _          => throw FreeVariableError(Receive(rep, chExp, bind, p))
      }

      case LetIn(name, exp, p) :: runTail =>
        this.withRun(substituteProc(p, name, evalExp(exp)) :: runTail).someOf

      case IfThenElse(exp, tP, fP) :: runTail => evalExp(exp) match {
        case EEBool(true) => this.withRun(tP :: runTail).someOf
        case EEBool(false) => this.withRun(fP :: runTail).someOf
        case _ => throw TypeError("if")
      }

      case Parallel(p, q) :: runTail =>
        this.withRun(p :: (runTail :+ q)).someOf

      case New(name, p) :: runTail => {
        val nu: Name = this.next.next
        val newP: Proc = substituteProc(p, name, EEChan(nu))
        this.withWait(nu, EmptyQueue)
          .withRun(newP :: runTail).withNext(nu)
          .someOf
      }

      case End :: runTail => this.withRun(runTail).someOf
    }
  }
}

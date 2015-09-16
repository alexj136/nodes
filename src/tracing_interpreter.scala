package tracing_interpreter

import syntax._
import interpreter.Evaluator._

object TracingInterpreter {

  sealed abstract class ProcQueue
  case class SendQueue(elems: List[(Exp, Proc)]) extends ProcQueue {
    def this(elems: List[(Exp, Proc)]): SendQueue = {
      if (elems.length == 0) throw new RuntimeException("Empty SendQueue")
      this(elems)
    }
  }
  case class ReceiveQueue(elems: List[(Option[Int], Name, Proc)])
    extends ProcQueue
  case object EmptyQueue extends ProcQueue

  class TracingMachineState(
      run: List[Proc],
      wait: Map[Name, ProcQueue],
      names: Map[Name, String],
      next: Name) extends MachineState(run, wait, names, next) {

    override def withRun(newRun: List[Proc]): MachineState =
      new TracingMachineState(newRun, this.wait, this.names, this.next)

    override def withWait(ch: Name, onCh: ProcQueue): MachineState =
      new TracingMachineState(
        this.run, this.wait.updated(ch, onCh), this.names, this.next)

    override def withNext(newNext: Name): MachineState =
      new TracingMachineState(this.run, this.wait, this.names, newNext)

    override def step: Option[MachineState] = this.run match {
      case Nil => None

      case Send(ChanLiteral(ch), msg, p) :: runTail
        if this.names.get(ch) == Some("$print") => {
          println(evalExp(msg).unEvalExp pstr this.names)
          this.withRun(p :: runTail).someOf
        }

      case Send(ChanLiteral(ch), msg, p) :: runTail => this.wait(ch) match {

        case Receive(repl, ChanLiteral(_), bind, q) :: moreRecs => {
          val qSub: Proc = substituteProc(q, bind, evalExp(msg))
          val waitTail: List[Proc] =
            if (repl) List(Receive(true, ChanLiteral(ch), bind, q)) else Nil
          this.withRun(p :: (runTail :+ qSub))
              .withWait(ch, moreRecs ++ waitTail)
              .someOf
        }
        case nilOrSends =>
          this.withRun(runTail)
              .withWait(ch, nilOrSends :+ Send(ChanLiteral(ch), msg, p))
              .someOf
      }

      case Send(chExp, msg, p) :: runTail => evalExp(chExp) match {
        case EEChan(ch) =>
          this.withRun(Send(ChanLiteral(ch), msg, p) :: runTail).someOf
        case _          => throw FreeVariableError(Send(chExp, msg, p))
      }

      case Receive(repl, ChanLiteral(ch), bind, p) :: runTail =>
        this.wait(ch) match {

          case Send(ChanLiteral(_), msg, q) :: moreSends => {
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
          case nilOrReceives =>
            this.withRun(runTail)
                .withWait(ch,
                  nilOrReceives :+ Receive(repl, ChanLiteral(ch), bind, p))
                .someOf
        }

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
        this.withWait(nu, Nil).withRun(newP :: runTail).withNext(nu).someOf
      }

      case End :: runTail => this.withRun(runTail).someOf
    }
  }
}

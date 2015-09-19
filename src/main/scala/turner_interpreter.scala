package turner_interpreter

import syntax._
import interpreter_common._
import interpreter_common.Functions._

class TurnerMachineState(
    run:   List[Proc], 
    wait:  Map[Name, List[Proc]],
    names: Map[Name, String],
    next:  Name) extends MachineState {

  override def toProc: Proc = (this.run :: this.wait.toList.map(_._2)).flatten
    .fold(End){ (p, q) => Parallel(p, q) }

  def withRun(newRun: List[Proc]): TurnerMachineState =
    new TurnerMachineState(newRun, this.wait, this.names, this.next)

  def withWait(ch: Name, onCh: List[Proc]): TurnerMachineState =
    new TurnerMachineState(
      this.run, this.wait.updated(ch, onCh), this.names, this.next)

  def withNext(newNext: Name): TurnerMachineState =
    new TurnerMachineState(this.run, this.wait, this.names, newNext)

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

    case Receive(repl, chExp, bind, p) :: runTail => evalExp(chExp) match {
      case EEChan(ch) =>
        this.withRun(Receive(repl, ChanLiteral(ch), bind, p) :: runTail).someOf
      case _          => throw FreeVariableError(Receive(repl, chExp, bind, p))
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

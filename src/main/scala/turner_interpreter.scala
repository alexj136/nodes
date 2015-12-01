package turner_interpreter

import syntax._
import interpreter._

class TurnerMachineState(
    run:   List[Proc], 
    wait:  Map[Name, List[Proc]],
    names: Map[Name, String],
    next:  Name) extends MachineState {

  override def toProc: Proc = (this.run :: this.wait.toList.map(_._2)).flatten
    .fold(End){ (p, q) => Parallel(p, q) }

  def withRun(newRun: List[Proc]): TurnerMachineState =
    new TurnerMachineState(newRun, this.wait, this.names, this.next)

  def runPrepend(newProc: Proc): TurnerMachineState =
    this.withRun(newProc :: this.run)

  def runAppend(newProc: Proc): TurnerMachineState =
    this.withRun(this.run :+ newProc)

  def withWait(ch: Name, onCh: List[Proc]): TurnerMachineState =
    new TurnerMachineState(
      this.run, this.wait.updated(ch, onCh), this.names, this.next)

  def splitWait(ch: Name): (Option[Proc], TurnerMachineState) =
    (this.wait(ch).headOption, this.withWait(ch, this.wait(ch) drop 1))

  def waitPrepend(ch: Name, proc: Proc): TurnerMachineState =
    this.withWait(ch, proc :: this.wait(ch))

  def waitAppend(ch: Name, proc: Proc): TurnerMachineState =
    this.withWait(ch, this.wait(ch) :+ proc)

  def withNext(newNext: Name): TurnerMachineState =
    new TurnerMachineState(this.run, this.wait, this.names, newNext)

  def makeName: (Name, TurnerMachineState) =
    (this.next.next, this.withNext(this.next.next))

  override def step: Option[MachineState] = this.run match {
    case Nil => None

    case Send(chExp, msg, p) :: runTail =>
      this.withRun(runTail).handleSend(Send(chExp, msg, p))

    case Receive(repl, chExp, bind, p) :: runTail =>
      this.withRun(runTail).handleReceive(Receive(repl, chExp, bind, p))

    case LetIn(name, exp, p) :: runTail =>
      this.withRun(runTail).handleLetIn(LetIn(name, exp, p))

    case IfThenElse(exp, tP, fP) :: runTail =>
      this.withRun(runTail).handleIfThenElse(IfThenElse(exp, tP, fP))

    case Parallel(p, q) :: runTail =>
      this.withRun(runTail).handleParallel(Parallel(p, q))

    case New(name, p) :: runTail =>
      this.withRun(runTail).handleNew(New(name, p))

    case End :: runTail => this.withRun(runTail).someOf
  }

  def handleSend(send: Send): Option[MachineState] = send match {
    case Send(ChanLiteral(ch), msg, p)
      if this.names.get(ch) == Some("$print") => {
        println((EvalExp from msg).unEvalExp pstr this.names)
        this.runPrepend(p).someOf
      }

    case Send(ChanLiteral(ch), msg, p) =>
      val (nextWait, thisWithoutNextWait): (Option[Proc], TurnerMachineState) =
        this.splitWait(ch)
      nextWait match {

      case Some(Receive(repl, ChanLiteral(_), bind, q)) => {
        val qSub: Proc = substituteProc(q, bind, EvalExp from msg)
        if (repl)
          thisWithoutNextWait
            .runPrepend(p)
            .runAppend(qSub)
            .waitAppend(ch, Receive(true, ChanLiteral(ch), bind, q))
            .someOf
        else
          thisWithoutNextWait
            .runPrepend(p)
            .runAppend(qSub)
            .someOf
      }
      case _ => this.waitAppend(ch, send).someOf
    }

    case Send(chExp, msg, p) => EvalExp from chExp match {
      case EEChan(ch) => this.runPrepend(Send(ChanLiteral(ch), msg, p)).someOf
      case _          => throw FreeVariableError(send)
    }
  }

  def handleReceive(receive: Receive): Option[MachineState] = receive match {
    case Receive(repl, ChanLiteral(ch), bind, p) =>
      val (nextWait, thisWithoutNextWait): (Option[Proc], TurnerMachineState) =
        this.splitWait(ch)
      nextWait match {

        case Some(Send(ChanLiteral(_), msg, q)) => {
          val pSub: Proc = substituteProc(p, bind, EvalExp from msg)
          if (repl)
            thisWithoutNextWait
              .runPrepend(receive)
              .runAppend(pSub)
              .runAppend(q)
              .someOf
          else
            thisWithoutNextWait
              .runPrepend(pSub)
              .runAppend(q)
              .someOf
        }
        case _ => this.waitAppend(ch, receive).someOf
      }

    case Receive(repl, chExp, bind, p) => EvalExp from chExp match {
      case EEChan(ch) =>
        this.runPrepend(Receive(repl, ChanLiteral(ch), bind, p)).someOf
      case _          => throw FreeVariableError(receive)
    }
  }

  def handleLetIn(letIn: LetIn): Option[MachineState] = letIn match {
    case LetIn(name, exp, p) =>
      this.runPrepend(substituteProc(p, name, EvalExp from exp)).someOf
  }

  def handleIfThenElse(iTE: IfThenElse): Option[MachineState] = iTE match {
    case IfThenElse(exp, tP, fP) => EvalExp from exp match {
      case EEBool(bool)  => this.runPrepend(if (bool) tP else fP).someOf
      case _             => throw TypeError("if")
    }
  }

  def handleParallel(par: Parallel): Option[MachineState] = par match {
    case Parallel(p, q) => this.runPrepend(p).runAppend(q).someOf
  }

  def handleNew(nu: New): Option[MachineState] = nu match {
    case New(name, p) => {
      val (nuV, thisWithNewName): (Name, TurnerMachineState) = this.makeName
      thisWithNewName
        .withWait(nuV, Nil)
        .runPrepend(substituteProc(p, name, EEChan(nuV)))
        .someOf
    }
  }
}

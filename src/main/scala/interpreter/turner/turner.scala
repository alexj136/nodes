package interpreter.turner

import syntax._
import interpreter._

object runWithTurnerMachine extends Function3[
    Proc,
    Map[Name, String],
    Name,
    (Proc, Map[Name, String], Name)
  ] {

  def apply(
      proc: Proc,
      names: Map[Name, String],
      next: Name)
  : (Proc, Map[Name, String], Name) = {
    var state: MachineState = new TurnerMachineState(proc.listify,
      names map {case (id, str) => (id, Nil)}, names, next)
    var stepState: Option[MachineState] = state.step
    while (stepState != None) {
      state     = stepState.get
      stepState = state.step
    }
    (state.toProc, state.getNames, state.getNext)
  }
}

class TurnerMachineState(
    run:   List[Proc], 
    wait:  Map[Name, List[Proc]],
    names: Map[Name, String],
    next:  Name) extends MachineState {

  override def toProc: Proc = (this.run :: this.wait.toList.map(_._2)).flatten
    .fold(End){ (p, q) => Parallel(p, q) }

  def getNames: Map[Name, String] = names
  def getNext: Name = next

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

    case Receive(repl, chExp, bind, ty, p) :: runTail =>
      this.withRun(runTail).handleReceive(Receive(repl, chExp, bind, ty, p))

    case LetIn(name, ty, exp, p) :: runTail =>
      this.withRun(runTail).handleLetIn(LetIn(name, ty, exp, p))

    case IfThenElse(exp, tP, fP) :: runTail =>
      this.withRun(runTail).handleIfThenElse(IfThenElse(exp, tP, fP))

    case Parallel(p, q) :: runTail =>
      this.withRun(runTail).handleParallel(Parallel(p, q))

    case New(name, ty, p) :: runTail =>
      this.withRun(runTail).handleNew(New(name, ty, p))

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

      case Some(Receive(repl, ChanLiteral(_), bind, ty, q)) => {
        val qSub: Proc = substituteProc(q, bind, EvalExp from msg)
        if (repl)
          thisWithoutNextWait
            .runPrepend(p)
            .runAppend(qSub)
            .waitAppend(ch, Receive(true, ChanLiteral(ch), bind, ty, q))
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
    case Receive(repl, ChanLiteral(ch), bind, ty, p) =>
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

    case Receive(repl, chExp, bind, ty, p) => EvalExp from chExp match {
      case EEChan(ch) =>
        this.runPrepend(Receive(repl, ChanLiteral(ch), bind, ty, p)).someOf
      case _          => throw FreeVariableError(receive)
    }
  }

  def handleLetIn(letIn: LetIn): Option[MachineState] = letIn match {
    case LetIn(name, _, exp, p) =>
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
    case New(name, _, p) => {
      val (nuV, thisWithNewName): (Name, TurnerMachineState) = this.makeName
      thisWithNewName
        .withWait(nuV, Nil)
        .runPrepend(substituteProc(p, name, EEChan(nuV)))
        .someOf
    }
  }
}

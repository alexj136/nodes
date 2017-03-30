package interpreter.turner

import syntax._
import interpreter._

object runWithTurnerMachine extends Function3[
    Proc,
    Map[Name, String],
    NumName,
    (Proc, Map[Name, String], NumName)
  ] {

  def apply(
      proc: Proc,
      names: Map[Name, String],
      next: NumName)
  : (Proc, Map[Name, String], NumName) = {
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
    next:  NumName) extends MachineState {

  override def toProc: Proc = (this.run :: this.wait.toList.map(_._2)).flatten
    .fold(End){ (p, q) => Parallel(p, q) }

  def getNames: Map[Name, String] = names
  def getNext: NumName = next

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

  def withNext(newNext: NumName): TurnerMachineState =
    new TurnerMachineState(this.run, this.wait, this.names, newNext)

  def makeName: (NumName, TurnerMachineState) =
    (this.next.next, this.withNext(this.next.next))

  override def step: Option[MachineState] = this.run match {
    case Nil => None

    case Send(c, ts, ms, p) :: runTail =>
      this.withRun(runTail).handleSend(Send(c, ts, ms, p))

    case Receive(r, c, qs, as, p) :: runTail =>
      this.withRun(runTail).handleReceive(Receive(r, c, qs, as, p))

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
    case Send(ChanLiteral(StdOutName), ts, ms, p) => {
      println(ms map { m =>
        ((EvalExp from m).unEvalExp pstr this.names) mkString ", " })
      this.runPrepend(p).someOf
    }

    case Send(ChanLiteral(c), ts, ms, p) =>
      val (nextWait, thisWithoutNextWait): (Option[Proc], TurnerMachineState) =
        this.splitWait(c)
      nextWait match {

        case Some(Receive(r, ChanLiteral(_), qs, as, q)) => {
          val bindsEvalExps: List[(Name, EvalExp)] =
            (as map (_._1)) zip (ms map (EvalExp from _))
          val qSub: Proc = substituteProcFold(q, bindsEvalExps)
          if (r)
            thisWithoutNextWait
              .runPrepend(p)
              .runAppend(qSub)
              .waitAppend(c, Receive(true, ChanLiteral(c), qs, as, q))
              .someOf
          else
            thisWithoutNextWait
              .runPrepend(p)
              .runAppend(qSub)
              .someOf
        }
        case _ => this.waitAppend(c, send).someOf
      }

    case Send(cE, ts, ms, p) => EvalExp from cE match {
      case EEChan(c) => this.runPrepend(Send(ChanLiteral(c), ts, ms, p)).someOf
      case _         => throw TypeError("send on non-channel expression")
    }
  }

  def handleReceive(receive: Receive): Option[MachineState] = receive match {
    case Receive(r, ChanLiteral(c), qs, as, p) =>
      val (nextWait, thisWithoutNextWait): (Option[Proc], TurnerMachineState) =
        this.splitWait(c)
      nextWait match {

        case Some(Send(ChanLiteral(_), ts, ms, q)) => {
          val bindsEvalExps: List[(Name, EvalExp)] =
            (as map (_._1)) zip (ms map (EvalExp from _))
          val pSub: Proc = substituteProcFold(p, bindsEvalExps)
          if (r)
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
        case _ => this.waitAppend(c, receive).someOf
      }

    case Receive(r, cE, qs, as, p) => EvalExp from cE match {
      case EEChan(c) =>
        this.runPrepend(Receive(r, ChanLiteral(c), qs, as, p)).someOf
      case _         => throw TypeError("receive on non-channel expression")
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
      val (nuV, thisWithNewName): (NumName, TurnerMachineState) = this.makeName
      thisWithNewName
        .withWait(nuV, Nil)
        .runPrepend(substituteProc(p, name, EEChan(nuV)))
        .someOf
    }
  }
}

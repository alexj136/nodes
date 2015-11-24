package runtime_conditional_forwarder_optimising_concurrent_interpreter

import akka.actor._
import syntax._
import interpreter_common._
import interpreter_common.Functions._
import concurrent_interpreter._

class RunTCondFwdOptProcRunner(
    _chanMap: Map[Name, ActorRef],
    _proc: Proc,
    procManager: ActorRef)
  extends ProcRunner(_chanMap, _proc, procManager) {

  private var optimisationAttempted: Boolean = false

  override def handleServer(chExp: Exp, bind: Name, p: Proc): Unit = {
    if (this.optimisationAttempted) super.handleServer(chExp, bind, p)
    else {
      this.proc = ???
      this.optimisationAttempted = true
      self ! ProcGo
    }
  }
}

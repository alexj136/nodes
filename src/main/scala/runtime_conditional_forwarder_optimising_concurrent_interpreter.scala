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

  def isServer: Boolean = this.proc match {
    case Receive(true, _, _, _) => true
    case _                      => false
  }

  override def handleServer(chExp: Exp, bind: Name, p: Proc): Unit = ???
}

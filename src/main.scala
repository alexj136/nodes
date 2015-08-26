package main

import syntax._
import interpreter.Evaluator._
import parser._
import java.io.File
import java.io.FileInputStream

object Main extends App {
  val file: File = new File(args(0))
  var stream: FileInputStream = new FileInputStream(file)
  val (proc, revNames, nextName): (Proc, Map[String, Name], Name) =
    Parser.parseStream(stream)
  val names: Map[Name, String] = revNames.map(_.swap)
  stream.close()
  var state: MachineState =
    new MachineState(proc.listify, names map
      {case (id, str) => (id, List(): List[Proc])}, nextName)
  var stepState: Option[MachineState] = state.step
  try {
    while (stepState != None) {
      state = stepState.get
      stepState = state.step
    }
    println(state.toProc pstr names)
  }
  catch {
    case TypeError         ( s ) => println(s"Type error in \'$s\'")
    case FreeVariableError ( s ) =>
      println(s"Unbound variable error: '${s pstr names}'")
  }
}

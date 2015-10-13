package main

import syntax._
import parser._
import interpreter_common._
import interpreter_common.Functions._
import tracing_interpreter._
import turner_interpreter._
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
    new /*Tracing*/TurnerMachineState(proc.listify /*map {p => Left(p)}*/, names map
      {case (id, str) => (id, Nil/*EmptyQueue*/)}, names, nextName)
  var stepState: Option[MachineState] = state.step
  try {
    while (stepState != None) {
      state = stepState.get
      stepState = state.step
    }
  }
  catch {
    case TypeError         ( s ) => println(s"Type error in \'$s\'")
    case FreeVariableError ( s ) =>
      println(s"Unbound variable error: '${s pstr names}'")
  }
}

class Args(val useConcurrency: Boolean = true, val inputFileName: String)

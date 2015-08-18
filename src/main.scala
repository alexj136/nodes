package main

import syntax._
import interpreter.Evaluator._
import parser._
import java.io.File
import java.io.FileInputStream

object Main extends App {
  val file: File = new File(args(0))
  var stream: FileInputStream = new FileInputStream(file)
  val parse: (Proc, scala.collection.Map[String, Name], Name) =
    Parser.parseStream(stream)
  stream.close()
  var state: MachineState =
    new MachineState(List(parse._1), Map.empty, parse._3)
  var stepState: Option[MachineState] = state.step
  while (stepState != None) {
    state = stepState.get
    stepState = stepState.step
  }
  println(parse._1.pstr(parse._2.map(_.swap).toMap))
}

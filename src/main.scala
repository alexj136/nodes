package main

import syntax._
//import interpreter._
import parser._

import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException

object Main extends App {
  val file: File = new File(args(0))
  var stream: FileInputStream = new FileInputStream(file)
  val proc: Proc = Parser.parseStream(stream)._1
  stream.close()
  println(proc)
}

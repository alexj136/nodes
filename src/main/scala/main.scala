package main

import syntax._
import parser._
import interpreter_common._
import interpreter_common.Functions._
import concurrent_interpreter._
import forwarder_optimising_concurrent_interpreter._
import java.io.File
import java.io.FileInputStream
import java.io.IOException

object Main extends App {

  if (args.length < 1) {
    println("Bad command, please supply a filename.")
    sys.exit(1)
  }

  val file: File = new File(args(0))

  if (!file.exists) {
    println(s"File '${file.getAbsolutePath}' does not exist.")
    sys.exit(1)
  }

  if (file.isDirectory) {
    println(s"File '${file.getAbsolutePath}' is a directory.")
    sys.exit(1)
  }

  try {

    var stream: FileInputStream = new FileInputStream(file)
    val (proc, revNames, nextName): (Proc, Map[String, Name], Name) =
      Parser.parseStream(stream)
    stream.close()
    new Launcher(proc, revNames("$print"), nextName, revNames.map(_.swap),
      { case _ => {} }, classOf[FwdOptProcManager])

  } catch {

    case ioe: IOException => {
      println("IO error.")
      sys.exit(1)
    }

    case e: Exception => {
      println("Unknown error:")
      e.printStackTrace
      sys.exit(1)
    }
  }
}

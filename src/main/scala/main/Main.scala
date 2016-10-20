package main

import syntax._
import parser._
import interpreter._
import interpreter.concurrent._
import interpreter.concurrent.forwarder_optimising.spawn_time._
import java.io.File
import java.io.IOException
import scala.io.Source

// Input to new lexer from file thusly:
//   newparser.Lexer.lex(new scala.util.parsing.input.PagedSeqReader(
//     scala.collections.immutable.PagedSeq.fromLines(
//       scala.io.Source.fromFile("filename"))))
object Main extends App {

  if (args.length < 1) {
    println("Bad command, please supply a filename.")
    sys.exit(1)
  }

  val file: java.io.File = new java.io.File ( args ( 0 ) )

  if (!file.exists) {
    println(s"File '${file.getAbsolutePath}' does not exist.")
    sys.exit(1)
  }

  if (file.isDirectory) {
    println(s"File '${file.getAbsolutePath}' is a directory.")
    sys.exit(1)
  }

  try {

    lexAndParse ( Parser.proc , Source fromFile args(0) ) match {
      case Right ( ( names , nextName , proc ) ) =>
        new Launcher(proc, names("$print"), nextName, names.map(_.swap),
          { case _ => {} }, classOf[FwdOptProcRunner])
      case Left ( LexerError  ( row , col , msg ) ) => {
        println("Lexical error (" + row + ", " + col + "): " + msg)
      }
      case Left ( ParserError ( row , col , msg ) ) => {
        println("Syntax error (" + row + ", " + col + "): " + msg)
      }
    }

  } catch {

    case ioe: IOException => {
      println("IO error.")
      sys.exit(1)
    }

    case e: Exception => {
      println(s"Unknown error: ${e.getClass.getName}")
      e.printStackTrace
      sys.exit(1)
    }
  }
}

package main

import syntax._
import parser._
import typecheck._
import interpreter._
import interpreter.concurrent._
import java.io.File
import java.io.IOException
import scala.io.Source

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

        // 'flip' the name map from the lexer such that we can easily use it to
        // print terms.
        val namesF: Map [ Name , String ] = names.map ( _.swap )

        // Generate typing constraints
        val ( _ , constr: ConstraintSet , _ ) = Typecheck.constraintsProc (
          proc , Map.empty , findNextName ( proc.free ) )

        // Try to solve constraints
        Typecheck.unify ( constr , ConstraintSet.empty ) match {

          // If constraints are solved, run the program
          case Right ( _  ) =>
            new Launcher(proc, names get "$print", nextName, names.map(_.swap),
              { case _ => {} }, classOf[ProcRunner])

          case Left  ( cs ) => {
            println ( s"${cs.size} type errors found:" )
            cs.foreach ( { c =>
              println (
                s"\n  Cannot unify ${c.t1 pstr namesF} " +
                s"with ${c.t2 pstr namesF}." )
              c.origins.foreach ( { o =>
                println ( s"    At ${o.info}:" )
                println ( s"    ${o pstr namesF}" )
              } )
            } )
          }

        }

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
      ioe.printStackTrace
      sys.exit(1)
    }

    case e: Exception => {
      println(s"Unknown error: ${e.getClass.getName}")
      e.printStackTrace
      sys.exit(1)
    }
  }
}

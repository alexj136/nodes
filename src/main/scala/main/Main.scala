package main

import syntax._
import parser._
import typecheck._
import interpreter._
import interpreter.turner.runWithTurnerMachine
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

        val checker: Typecheck = new Typecheck ( nextName )

        // Generate typing constraints
        val ( _ , constr: ConstraintSet ) =
          checker.constraintsProc ( proc , Map.empty )

        // Try to solve constraints
        checker.unify ( constr , ConstraintSet.empty ) match {

          // If constraints are solved, run the program
          case Right ( _  ) =>
            val (procAfter: Proc, namesFAfter: Map[Name, String], _) =
              runWithTurnerMachine(proc, namesF, nextName)
            println ( procAfter.noServers pstr namesFAfter )
            /*new Launcher(proc, names get "$print", nextName, namesF,
              { case _ => {} }, classOf[ProcRunner])*/

          case Left  ( cs ) => {
            println ( s"${cs.size} type errors found:" )
            cs.foreach ( {
              case TypeConstraint ( t1 , t2 , orig ) => {
                println (
                  s"\n  Cannot unify ${t1 pstr namesF} " +
                  s"with ${t2 pstr namesF}." )
                orig.foreach ( { o =>
                  println ( s"    At ${o.info}:" )
                  println ( s"    ${o pstr namesF}" )
                } )
              }
              case ArityConstraint ( a1 , a2 , orig ) => {
                println ( s"\n  Arity mismatch $a1 != $a2." )
                orig.foreach ( { o =>
                  println ( s"    At ${o.info}:" )
                  println ( s"    ${o pstr namesF}" )
                } )
              }
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

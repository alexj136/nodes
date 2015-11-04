package parser

import syntax._
import java_cup.runtime.ComplexSymbolFactory.Location
import java.util.Scanner
import java.io.File
import java.io.StringWriter
import java.io.PrintWriter

sealed abstract class ParserResult

case class ParserSuccess(
    p: Proc,
    names: Map[String, Name],
    nextName: Name)
  extends ParserResult

case class SyntaxErrors(errors: List[SyntaxError]) extends ParserResult

class SyntaxError(
    startRow: Int,
    startCol: Int,
    endRow: Int,
    endCol: Int,
    text: String) {

  def this(start: Location, end: Location, fileText: List[String]) =
    this(start.getLine, start.getColumn, end.getLine, end.getColumn, ???)

  override def toString: String = "Syntax error spanning from line " ++
    s"$startRow, column $startCol to line $endRow, column $endCol"
}

case class FileNotFound    ( fileName: String ) extends ParserResult
case class FileIsDirectory ( fileName: String ) extends ParserResult
case class FileIOError     ( fileName: String ) extends ParserResult

case class UnknownError(
    fileName: String,
    stackTrace: String)
  extends ParserResult {

    def this(fileName: String, exc: Exception) = this(fileName, {
      val errors: StringWriter = new StringWriter
      exc.printStackTrace(new PrintWriter(errors))
      errors.toString
    })
}

object readToList {
  def apply(file: File): List[String] = {
    val scanner: Scanner = new Scanner(file)
    var lines: List[String] = Nil
    while (scanner.hasNextLine) lines = scanner.nextLine :: lines
    lines
  }
}

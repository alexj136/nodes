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

case class SyntaxErrors(errors: List[SyntaxError]) extends ParserResult {
  def toStringWithText(file: File): String = ???
}

class SyntaxError(
    val startRow: Int,
    val startCol: Int,
    val endRow: Int,
    val endCol: Int) {

  def isBefore(other: SyntaxError): Boolean =
    if (this.startRow == other.startRow)
      this.startCol < other.endCol
    else
      this.startRow < other.startRow

  def this(start: Location, end: Location) =
    this(start.getLine, start.getColumn, end.getLine, end.getColumn)

  override def toString: String = "Syntax error spanning from line " ++
    s"$startRow, column $startCol to line $endRow, column $endCol"
}

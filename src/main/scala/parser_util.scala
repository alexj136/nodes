package parser

import syntax._
import java_cup.runtime.ComplexSymbolFactory.Location
import java.util.Scanner
import java.io.File
import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source

sealed abstract class ParserResult

case class ParserSuccess(
    p: Proc,
    names: Map[String, Name],
    nextName: Name)
  extends ParserResult

case class SyntaxErrors(errors: List[SyntaxError]) extends ParserResult {


  def toStringWithText(file: File): List[String] = {

    def spacesAndUpArrows(
        lineLength: Int,
        startArrows: Int,
        endArrows: Int)
      : String =
      ( (for (i <- List.range( 0           , startArrows )) yield ' ') ++
        (for (i <- List.range( startArrows , endArrows   )) yield '^') ++
        (for (i <- List.range( endArrows   , lineLength  )) yield ' ')
      ).mkString

    def textSpan(text: List[String], span: SyntaxError): List[String] = {
      val spanLines: List[String] = text.slice(span.startRow, span.endRow)
      spanLines match {
        case         Nil => throw new RuntimeException("Bad location")
        case line :: Nil => List(line, spacesAndUpArrows(
          text(span.startRow).length, span.startCol, span.endCol))
        case line :: lns => ???
      }
    }

    val source: Source = Source.fromFile(file)
    val lines: List[String] = try source.getLines.toList finally source.close()
    (errors map (err => textSpan(lines, err))).flatten
  }
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

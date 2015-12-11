package parser

import syntax._
import java_cup.runtime.ComplexSymbolFactory.Location
import java.util.Scanner
import java.io.File
import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source

sealed abstract class ParserResult

case class ParserSuccess             (
    val p        : Proc              ,
    val names    : Map[String, Name] ,
    val nextName : Name              )
  extends ParserResult

case class SyntaxErrors(errors: List[SyntaxError]) extends ParserResult {


  def toStringWithText(file: File): List[String] = {

    def pointerLine        (
        lineLength  : Int  ,
        startArrows : Int  ,
        endArrows   : Int  ,
        pointChar   : Char ,
        otherChar   : Char )
      : String =
      ( (for (i <- List.range( 0           , startArrows )) yield otherChar) ++
        (for (i <- List.range( startArrows , endArrows   )) yield pointChar) ++
        (for (i <- List.range( endArrows   , lineLength  )) yield otherChar)
      ).mkString

    def textSpan(text: List[String], span: SyntaxError): List[String] = {
      val spanLines: List[String] = text.slice(span.startRow, span.endRow + 1)
      spanLines match {
        case         Nil => throw new RuntimeException("Bad location")
        case line :: Nil => List(line, pointerLine(text(span.startRow).length,
                                         span.startCol, span.endCol, '^', ' '))
        case       lines => lines map (s => "> " concat s)
      }
    }

    val source : Source       = Source.fromFile(file)
    val lines  : List[String] = try source.getLines.toList finally source.close()
    (errors map (err => err.toString :: textSpan(lines, err))).flatten
  }
}

class SyntaxError      (
    val startRow : Int ,
    val startCol : Int ,
    val endRow   : Int ,
    val endCol   : Int ) {

  def isBefore(other: SyntaxError): Boolean =
    if (this.startRow == other.startRow)
      this.startCol < other.endCol
    else
      this.startRow < other.startRow

  def this(start: Location, end: Location) =
    this(start.getLine, start.getColumn, end.getLine,
      if ((start.getLine > end.getLine) ||
          ((start.getLine == end.getLine) && (start.getColumn > end.getColumn)))
        throw new IllegalArgumentException("start not before end")
      else end.getColumn
    )

  override def toString: String = "Syntax error spanning from line " ++
    s"${startRow + 1}, column ${startCol + 1} to line " ++
    s"${endRow + 1}, column ${endCol + 1}:"
}

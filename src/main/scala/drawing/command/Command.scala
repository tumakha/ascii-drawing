package drawing.command

import drawing.command.Command.defaultBrush

import scala.util.Try
import scala.util.matching.Regex

/**
  * @author Yuriy Tumakha
  */
object Command {

  val canvasPattern: Regex = """C\s+(\d{1,3})\s+(\d{1,3})""".r
  val linePattern: Regex = """L\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r
  val rectanglePattern: Regex = """R\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r
  val defaultBrush: Char = 'x'

  def parse(line: String): Try[Command] =
    Try(
      line match {
        case canvasPattern(width, height) => Canvas(width.toInt, height.toInt)
        case linePattern(x1, y1, x2, y2) if x1 == x2 || y1 == y2 =>
          Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
        case linePattern(_, _, _, _) =>
          throw new IllegalArgumentException("Only horizontal or vertical lines are supported")
        case rectanglePattern(x1, y1, x2, y2) if x1 <= x2 && y1 <= y2 =>
          Rectangle(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
        case rectanglePattern(_, _, _, _) =>
          throw new IllegalArgumentException("Rectangle upper left corner coordinates should be specified firstly")
        case "U" => Undo
        case "Q" => Quit
        case _ => throw new IllegalArgumentException("Wrong command format")
      }
    )

}

case class Point(x: Int, y: Int)

sealed trait Command

case class Canvas(width: Int, height: Int) extends Command

case class Line(point1: Point, point2: Point, brush: Char = defaultBrush) extends Command {
  val horizontal: Boolean = point1.y == point2.y
  val vertical: Boolean = point1.x == point2.x
}

case class Rectangle(point1: Point, point2: Point, brush: Char = defaultBrush) extends Command

case object Quit extends Command

case object Undo extends Command

case object Empty extends Command

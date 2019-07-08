package drawing.command

import scala.util.matching.Regex

/**
  * @author Yuriy Tumakha
  */
object Command {

  val canvasPattern: Regex = """C\s+(\d{1,3})\s+(\d{1,3})""".r
  val linePattern: Regex = """L\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r
  val rectanglePattern: Regex = """R\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r

  def parse(line: String): Either[String, Command] =
    line match {
      case canvasPattern(width, height) => Right(Canvas(width.toInt, height.toInt))
      case linePattern(x1, y1, x2, y2) if x1 == x2 || y1 == y2 =>
        Right(Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
      case linePattern(_, _, _, _) =>
        Left("Only horizontal or vertical lines are supported")
      case rectanglePattern(x1, y1, x2, y2) if x1 <= x2 && y1 <= y2 =>
        Right(Rectangle(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
      case rectanglePattern(_, _, _, _) =>
        Left("Rectangle upper left corner coordinates should be specified firstly")
      case "Q" => Right(Quit)
      case _ => Left("Wrong command format")
    }

}

sealed trait Command

case class Canvas(width: Int, height: Int) extends Command

case class Line(x1: Int, y1: Int, x2: Int, y2: Int, brush: Char = 'x') extends Command {
  val horizontal: Boolean = y1 == y2
  val vertical: Boolean = x1 == x2
}

case class Rectangle(x1: Int, y1: Int, x2: Int, y2: Int, brush: Char = 'x') extends Command

case object Quit extends Command

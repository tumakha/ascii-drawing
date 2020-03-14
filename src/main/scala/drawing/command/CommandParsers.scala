package drawing.command

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
  * @author Yuriy Tumakha
  */
object CommandParsers extends RegexParsers {

  def digits: Parser[String] = """\d{1,3}""".r

  def num: Parser[Int] = digits ^^ (d => d.toInt)

  def pointsPair: Parser[(Point, Point)] = num ~ num ~ num ~ num ^^ {
    case x1 ~ y1 ~ x2 ~ y2 => (Point(x1, y1), Point(x2, y2))
  }

  def canvas: Parser[Canvas] = "C" ~ num ~ num ^^ {
    case "C" ~ width ~ height => Canvas(width, height)
  }

  def line: Parser[Line] = "L" ~ pointsPair ^^ {
    case "L" ~ points => Line(points._1, points._2)
  }

  def rectangle: Parser[Rectangle] = "R" ~ pointsPair ^^ {
    case "R" ~ points => Rectangle(points._1, points._2)
  }

  def undo: Parser[Undo.type] = """U""".r ^^ (_ => Undo)

  def quit: Parser[Quit.type] = """Q""".r ^^ (_ => Quit)

  def command: Parser[Command] = canvas | line | rectangle | undo | quit

  def parse(input: String): Try[Command] =
    Try(
      parseAll(command, input) match {
        case Success(com, _) => com
        case f: NoSuccess => throw new IllegalArgumentException("Wrong command format. " + f.msg)
      }
    )

}

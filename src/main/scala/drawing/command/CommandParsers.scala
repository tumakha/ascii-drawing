package drawing.command

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/**
  * @author Yuriy Tumakha
  */
object CommandParsers extends RegexParsers {

  def digits: Parser[String] = """\d{1,3}""".r

  def num: Parser[Int] = digits ^^ (d => d.toInt)

  def point: Parser[Point] = num ~ num ^^ {
    case x ~ y => Point(x, y)
  }

  def canvas: Parser[Canvas] = "C" ~ num ~ num ^^ {
    case "C" ~ width ~ height => Canvas(width, height)
  }

  def line: Parser[Line] = "L" ~ point ~ point ^^ {
    case "L" ~ point1 ~ point2 => Line(point1, point2)
  }

  def rectangle: Parser[Rectangle] = "R" ~ point ~ point ^^ {
    case "R" ~ point1 ~ point2 => Rectangle(point1, point2)
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

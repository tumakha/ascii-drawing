package drawing.command

import scala.util.matching.Regex

/**
  * @author Yuriy Tumakha
  */
object Line {
  val pattern: Regex = """L\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r
}

case class Line(x1: Int, y1: Int, x2: Int, y2: Int, brush: Char = 'x') extends Command {

  val horizontal: Boolean = y1 == y2

  val vertical: Boolean = x1 == x2

}

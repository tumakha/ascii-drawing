package drawing.command

import scala.util.matching.Regex

/**
  * @author Yuriy Tumakha
  */
object Rectangle {
  val pattern: Regex = """R\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})""".r
}

case class Rectangle(x1: Int, y1: Int, x2: Int, y2: Int, brush: Char = 'x') extends Command {

}

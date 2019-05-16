package drawing.command

import scala.util.matching.Regex

/**
  * @author Yuriy Tumakha
  */
object Canvas {
  val pattern: Regex = """C\s+(\d{1,3})\s+(\d{1,3})""".r
}

case class Canvas(width: Int, height: Int) extends Command {

}

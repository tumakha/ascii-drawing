package drawing.command

/**
 * @author Yuriy Tumakha
 */
sealed trait Command

case class Point(x: Int, y: Int)

case class Canvas(width: Int, height: Int) extends Command

case class Line(point1: Point, point2: Point) extends Command {
  val horizontal: Boolean = point1.y == point2.y
  val vertical: Boolean = point1.x == point2.x

  require(horizontal || vertical, "Only horizontal or vertical lines are supported")
}

case class Rectangle(point1: Point, point2: Point) extends Command {
  require(point1.x <= point2.x && point1.y <= point2.y,
    "Rectangle upper left corner coordinates should be specified firstly")
}

case object Quit extends Command

case object Undo extends Command

case object Empty extends Command

package drawing.screen

import drawing.command._
import drawing.screen.Screen.Content

import scala.util.Try

/**
 * @author Yuriy Tumakha
 */
class Drawing(val initScreen: Screen) {

  val defaultBrush: Char = 'x'
  var screen: Screen = initScreen

  type Canvas = Array[Array[Char]]

  implicit def Canvas2Content(canvas: Canvas): Content = canvas.map(_.mkString).mkString("\n")

  private lazy val canvas: Canvas =
    if (screen.content.isEmpty) throw new IllegalStateException("Canvas is not created")
    else screen.content.split("\n").map(_.toCharArray)

  private lazy val maxX: Int = canvas(0).length - 2
  private lazy val maxY: Int = canvas.length - 2

  def draw(command: Command): Try[Screen] =
    Try {
      screen = newScreen(command)
      screen
    }

  def newScreen(command: Command): Screen = {
    val content: Content = command match {
      case Canvas(width, height) => createCanvas(width, height)
      case line: Line => drawLine(line)
      case rectangle: Rectangle => drawRectangle(rectangle)
      case Quit | Empty => ""
      case Undo => return screen.prev
    }
    DrawingScreen(command, content, screen)
  }

  private def createCanvas(width: Int, height: Int): Canvas = {
    def initialValue(i: Int, j: Int): Char =
      if (i == 0 || i == height + 1) '-'
      else if (j == 0 || j == width + 1) '|'
      else ' '

    val maxWidth = Screen.defaultTerminalWidth
    val maxHeight = Screen.defaultTerminalHeight

    if (width < 0 || width > maxWidth)
      throw new IllegalArgumentException(s"Canvas width should be in range 0..$maxWidth")
    else if (height < 0 || height > maxHeight)
      throw new IllegalArgumentException(s"Canvas height should be in range 0..$maxHeight")

    Array.tabulate(height + 2, width + 2)(initialValue)
  }

  private def drawLine(line: Line): Canvas = drawingByJoinPoints(line.point1, line.point2)

  private def drawRectangle(rec: Rectangle): Canvas = {
    val point21 = Point(rec.point2.x, rec.point1.y)
    val point12 = Point(rec.point1.x, rec.point2.y)

    drawingByJoinPoints(rec.point1, point21, rec.point2, point12, rec.point1)
  }

  private def drawingByJoinPoints(point: Point*): Canvas = {
    def drawSingleLine(point1: Point, point2: Point): Point = {
      for {
        x <- rangeAsc(point1.x, point2.x)
        y <- rangeAsc(point1.y, point2.y)
      } canvas(safeY(y))(safeX(x)) = defaultBrush
      point2
    }

    checkAnyPointInsideCanvas(point: _*)
    point.reduce(drawSingleLine)
    canvas
  }

  private def safeX(x: Int): Int = safeInt(x, maxX)

  private def safeY(y: Int): Int = safeInt(y, maxY)

  private def safeInt(n: Int, max: Int): Int =
    if (n < 1) 1
    else if (n > max) max
    else n

  private def checkAnyPointInsideCanvas(point: Point*): Unit = {
    val points = point.distinct
    points.find(p => xInsideCanvas(p.x) && yInsideCanvas(p.y))
      .orElse(throw new IllegalArgumentException(s"All points are outside canvas area. ${points.toList.sortBy(_.x)}"))
  }

  private def xInsideCanvas(x: Int): Boolean = insideCanvas(x, maxX)

  private def yInsideCanvas(x: Int): Boolean = insideCanvas(x, maxY)

  private def insideCanvas(value: Int, max: Int): Boolean = value > 0 && value <= max

  private def rangeAsc(start: Int, end: Int): Range = if (start < end) start to end else end to start

}

package drawing.screen

import drawing.command._
import drawing.screen.Screen.Content

/**
  * @author Yuriy Tumakha
  */
case class Drawing(screen: Screen, command: Command) {

  type Canvas = Array[Array[Char]]

  implicit def Canvas2Content(canvas: Canvas): Content = canvas.map(_.mkString).mkString("\n")

  private lazy val canvas: Canvas =
    if (screen.content.isEmpty) throw new IllegalStateException("Canvas is not created")
    else screen.content.split("\n").map(_.toCharArray)

  private lazy val maxX: Int = canvas(0).length - 2
  private lazy val maxY: Int = canvas.length - 2

  def draw(): Screen = {
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

  private def drawLine(line: Line): Canvas = {
    if (line.horizontal) {
      checkYCoordinate(line.point1.y)
      rangeAsc(line.point1.x, line.point2.x).foreach(x => canvas(line.point1.y)(safeX(x)) = line.brush)
    } else if (line.vertical) {
      checkXCoordinate(line.point1.x)
      rangeAsc(line.point1.y, line.point2.y).foreach(y => canvas(safeY(y))(line.point1.x) = line.brush)
    }
    canvas
  }

  private def drawRectangle(rec: Rectangle): Canvas = {
    checkXCoordinate(rec.point1.x)
    checkXCoordinate(rec.point2.x)
    checkYCoordinate(rec.point1.y)
    checkYCoordinate(rec.point2.y)

    val point21 = Point(rec.point2.x, rec.point1.y)
    val point12 = Point(rec.point1.x, rec.point2.y)

    drawLine(Line(rec.point1, point21, rec.brush))
    drawLine(Line(point21, rec.point2, rec.brush))
    drawLine(Line(rec.point2, point12, rec.brush))
    drawLine(Line(point12, rec.point1, rec.brush))
  }

  private def safeX(x: Int): Int = safeInt(x, maxX)

  private def safeY(y: Int): Int = safeInt(y, maxY)

  private def safeInt(n: Int, max: Int): Int =
    if (n < 1) 1
    else if (n > max) max
    else n

  private def checkXCoordinate(value: Int): Unit = checkCoordinate('x', value, maxX)
  private def checkYCoordinate(value: Int): Unit = checkCoordinate('y', value, maxY)

  private def checkCoordinate(label: Char, value: Int, max: Int): Unit =
    if (value < 1 || value > max)
      throw new IllegalArgumentException(s"Coordinate $label = $value is outside canvas area")

  private def rangeAsc(start: Int, end: Int): Range = if (start < end) start to end else end to start

}

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
    checkAnyPointInsideCanvas(line.point1, line.point2)
    drawSingleLine(line)
    canvas
  }

  private def drawSingleLine(line: Line): Unit =
    if (line.horizontal) {
      rangeAsc(line.point1.x, line.point2.x).foreach(x => canvas(safeY(line.point1.y))(safeX(x)) = line.brush)
    } else if (line.vertical) {
      rangeAsc(line.point1.y, line.point2.y).foreach(y => canvas(safeY(y))(safeX(line.point1.x)) = line.brush)
    }

  private def drawRectangle(rec: Rectangle): Canvas = {
    val point21 = Point(rec.point2.x, rec.point1.y)
    val point12 = Point(rec.point1.x, rec.point2.y)

    checkAnyPointInsideCanvas(rec.point1, rec.point2, point12, point21)

    drawSingleLine(Line(rec.point1, point21, rec.brush))
    drawSingleLine(Line(point21, rec.point2, rec.brush))
    drawSingleLine(Line(rec.point2, point12, rec.brush))
    drawSingleLine(Line(point12, rec.point1, rec.brush))
    canvas
  }

  private def safeX(x: Int): Int = safeInt(x, maxX)
  private def safeY(y: Int): Int = safeInt(y, maxY)

  private def safeInt(n: Int, max: Int): Int =
    if (n < 1) 1
    else if (n > max) max
    else n

  private def checkAnyPointInsideCanvas(point: Point*): Unit =
    point.find(p => xInsideCanvas(p.x) && yInsideCanvas(p.y))
      .orElse(throw new IllegalArgumentException(s"All points are outside canvas area. ${point.toList}"))

  private def xInsideCanvas(x: Int): Boolean = insideCanvas(x, maxX)
  private def yInsideCanvas(x: Int): Boolean = insideCanvas(x, maxY)
  private def insideCanvas(value: Int, max: Int): Boolean = value > 0 && value <= max

  private def rangeAsc(start: Int, end: Int): Range = if (start < end) start to end else end to start

}

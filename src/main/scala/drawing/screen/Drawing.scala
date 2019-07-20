package drawing.screen

import drawing.command._
import drawing.screen.Screen.Content

/**
  * @author Yuriy Tumakha
  */
case class Drawing(screen: Screen, command: Command) {

  type Canvas = Array[Array[Char]]

  lazy val canvas: Canvas = getCanvas

  def draw(): Screen = {
    val content = command match {
      case Canvas(width, height) => createCanvas(width, height)
      case line: Line => drawLine(line)
      case rectangle: Rectangle => drawRectangle(rectangle)
      case Quit => ""
      case Undo => return screen.prev
      case Empty => screen.content
    }
    DrawingScreen(command, content, screen)
  }

  private def getCanvas: Canvas =
    if (screen.content.isEmpty) throw new IllegalStateException("Canvas is not created")
    else screen.content.split("\n").map(_.toCharArray)

  private def toContent(canvas: Canvas): Content = canvas.map(_.mkString).mkString("\n")

  private def createCanvas(width: Int, height: Int): Content = {
    def initialValue(i: Int, j: Int): Char = {
      if (i == 0 || i == height + 1) '-'
      else if (j == 0 || j == width + 1) '|'
      else ' '
    }

    val maxWidth = Screen.defaultTerminalWidth
    val maxHeight = Screen.defaultTerminalHeight

    if (width < 0 || width > maxWidth)
      throw new IllegalArgumentException(s"Canvas width should be in range 0..$maxWidth")
    else if (height < 0 || height > maxHeight)
      throw new IllegalArgumentException(s"Canvas height should be in range 0..$maxHeight")

    toContent(Array.tabulate(height + 2, width + 2)(initialValue))
  }

  private def drawLine(line: Line): String = {
    if (line.horizontal) {
      checkCoordinate('y', line.y1, maxY)
      val l = Seq(line.x1, line.x2).sorted
      for (x <- l.head to l.last) {
        canvas(line.y1)(safeX(x)) = line.brush
      }
    } else if (line.vertical) {
      checkCoordinate('x', line.x1, maxX)
      val l = Seq(line.y1, line.y2).sorted
      for (y <- l.head to l.last) {
        canvas(safeY(y))(line.x1) = line.brush
      }
    }
    toContent(canvas)
  }

  private def drawRectangle(rec: Rectangle): String = {
    checkCoordinate('x', rec.x1, maxX)
    checkCoordinate('x', rec.x2, maxX)
    checkCoordinate('y', rec.y1, maxY)
    checkCoordinate('y', rec.y2, maxY)

    drawLine(Line(rec.x1, rec.y1, rec.x2, rec.y1, rec.brush))
    drawLine(Line(rec.x2, rec.y1, rec.x2, rec.y2, rec.brush))
    drawLine(Line(rec.x2, rec.y2, rec.x1, rec.y2, rec.brush))
    drawLine(Line(rec.x1, rec.y2, rec.x1, rec.y1, rec.brush))
  }

  private def maxX: Int = canvas(0).length - 2

  private def maxY: Int = canvas.length - 2

  private def safeX(x: Int): Int = safeInt(x, maxX)

  private def safeY(y: Int): Int = safeInt(y, maxY)

  private def safeInt(n: Int, max: Int): Int =
    if (n < 1) 1
    else if (n > max) max
    else n

  private def checkCoordinate(label: Char, value: Int, max: Int): Unit =
    if (value < 1 || value > max)
      throw new IllegalArgumentException(s"Coordinate $label = $value is outside canvas area")

}

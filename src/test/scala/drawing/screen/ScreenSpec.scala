package drawing.screen

import drawing.command.{Canvas, Line, Quit, Rectangle, Undo}
import org.scalatest._

import scala.io.Source


class ScreenSpec() extends FlatSpec with Matchers {

  "Screen" should "create canvas with specified size" in {
    val screen0 = EmptyScreen

    val command1 = Canvas(20, 5)
    val screen1 = screen0.draw(command1).get
    screen1 shouldBe DrawingScreen(command1, getContent("emptyCanvas20x5.txt"), screen0)

    val command2 = Canvas(3, 7)
    val screen2 = screen1.draw(command2).get
    screen2 shouldBe DrawingScreen(command2, getContent("emptyCanvas3x7.txt"), screen1)

    val screen3 = screen2.draw(Canvas(1, 1)).get
    screen3.content shouldBe getContent("emptyCanvas1x1.txt")

    val screen4 = screen3.draw(Canvas(0, 0)).get
    screen4.content shouldBe getContent("emptyCanvas0x0.txt")

    val screen5 = screen4.draw(Canvas(130, 40)).get
    screen5.content shouldBe getContent("emptyCanvas130x40.txt")
  }

  it should "return Failure(exception) if height > maxHeight" in {
    val res = EmptyScreen.draw(Canvas(1, 50))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas height should be in range 0..${Screen.defaultTerminalHeight}"
  }

  it should "return Failure(exception) if width > maxWidth" in {
    val res = EmptyScreen.draw(Canvas(200, 2))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas width should be in range 0..${Screen.defaultTerminalWidth}"
  }

  it should "return Failure(exception) on draw line before Canvas created" in {
    val res = EmptyScreen.draw(Line(1, 3, 7, 3))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas is not created"
  }

  it should "draw horizontal line" in {
    EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(1, 3, 7, 3)).get.content shouldBe getContent("horizLine.txt")
  }

  it should "draw trimmed horizontal lines" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(0, 3, 7, 3)).get
    val command = Line(12, 1, 30, 1)
    screen.draw(command).get shouldBe DrawingScreen(command, getContent("trimmedHorizLines.txt"), screen)
  }

  it should "return Failure(exception) if Horizontal line coordinate y is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val y = 90
    val command = Line(12, y, 30, y)
    val res = screen.draw(command)
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Coordinate y = $y is outside canvas area"
  }

  it should "draw vertical line" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Line(5, 2, 5, 3)).get.content shouldBe getContent("vertLine.txt")
  }

  it should "draw trimmed vertical lines" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(5, 0, 5, 3)).get
    screen.draw(Line(13, 5, 13, 50)).get.content shouldBe getContent("trimmedVertLines.txt")
  }

  it should "return Failure(exception) if Vertical line coordinate x is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val x = 200
    val res = screen.draw(Line(x, 1, x, 5))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Coordinate x = $x is outside canvas area"
  }

  it should "draw line contains single point" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Line(12, 3, 12, 3)).get.content shouldBe getContent("singlePointLine.txt")
  }

  it should "draw rectangle" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Rectangle(5, 2, 17, 5)).get.content shouldBe getContent("rectangle.txt")
  }

  it should "return Failure(exception) if any rectangle coordinate x is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val x = 21
    val res = screen.draw(Rectangle(5, 2, x, 5))
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual s"Coordinate x = $x is outside canvas area"
  }

  it should "return Failure(exception) if any rectangle coordinate y is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val y = 10
    val res = screen.draw(Rectangle(5, 4, 12, y))
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual s"Coordinate y = $y is outside canvas area"
  }

  it should "undo last operation by Undo command" in {
    val screen0 = EmptyScreen

    val command1 = Canvas(20, 5)
    val screen1 = screen0.draw(command1).get
    screen1 shouldBe DrawingScreen(command1, getContent("emptyCanvas20x5.txt"), screen0)

    val command2 = Line(5, 2, 5, 3)
    val screen2 = screen1.draw(command2).get
    screen2 shouldBe DrawingScreen(command2, getContent("vertLine.txt"), screen1)

    val screen3 = screen2.draw(Undo).get
    screen3 shouldBe DrawingScreen(command1, getContent("emptyCanvas20x5.txt"), screen0)

    val screen4 = screen3.draw(Undo).get
    screen4 shouldBe EmptyScreen

    val res = screen4.draw(Undo)
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual "Commands history is empty"
  }

  it should "skip Quit command" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Quit).get.content shouldBe ""
  }

  private def getContent(file: String): String = {
    val source = Option(getClass.getClassLoader.getResource(file)).map(Source.fromURL(_))
    if (source.isEmpty) fail(s"File not found: $file")

    val src = source.get
    try {
      src.getLines().mkString("\n")
    } finally {
      src.close
    }
  }

}

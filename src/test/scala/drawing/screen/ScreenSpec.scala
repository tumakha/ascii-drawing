package drawing.screen

import drawing.command.{Canvas, Line, Point, Quit, Rectangle, Undo}
import org.scalatest._

import scala.io.Source

/**
  * @author Yuriy Tumakha
  */
class ScreenSpec extends FlatSpec with Matchers {

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
    val res = EmptyScreen.draw(Line(Point(1, 3), Point(7, 3)))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas is not created"
  }

  it should "draw horizontal line" in {
    EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(Point(1, 3), Point(7, 3))).get.content shouldBe getContent("horizLine.txt")
  }

  it should "draw trimmed horizontal lines" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(Point(0, 3), Point(7, 3))).get
    val command = Line(Point(12, 1), Point(30, 1))
    screen.draw(command).get shouldBe DrawingScreen(command, getContent("trimmedHorizLines.txt"), screen)
  }

  it should "return Failure(exception) if Horizontal line coordinate y is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val y = 90
    val command = Line(Point(12, y), Point(30, y))
    val res = screen.draw(command)
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual "All points are outside canvas area. List(Point(12,90), Point(30,90))"
  }

  it should "draw vertical line" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Line(Point(5, 2), Point(5, 3))).get.content shouldBe getContent("vertLine.txt")
  }

  it should "draw trimmed vertical lines" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get.draw(Line(Point(5, 0), Point(5, 3))).get
    screen.draw(Line(Point(13, 5), Point(13, 50))).get.content shouldBe getContent("trimmedVertLines.txt")
  }

  it should "return Failure(exception) if Vertical line coordinate x is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val x = 200
    val res = screen.draw(Line(Point(x, 1), Point(x, 5)))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual "All points are outside canvas area. List(Point(200,1), Point(200,5))"
  }

  it should "draw line contains single point" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Line(Point(12, 3), Point(12, 3))).get.content shouldBe getContent("singlePointLine.txt")
  }

  it should "draw rectangle" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    screen.draw(Rectangle(Point(5, 2), Point(17, 5))).get.content shouldBe getContent("rectangle.txt")
  }

  it should "draw rectangle partially if any rectangle coordinate x is inside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val x = 21
    val res = screen.draw(Rectangle(Point(5, 2), Point(x, 5)))
    res.isSuccess shouldBe true
  }

  it should "return Failure(exception) if all rectangle corners are outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val res = screen.draw(Rectangle(Point(22, 7), Point(25, 8)))
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual "All points are outside canvas area. List(Point(22,7), Point(22,8), Point(25,7), Point(25,8))"
  }

  it should "draw rectangle partially if any rectangle coordinate y is outside canvas area" in {
    val screen = EmptyScreen.draw(Canvas(20, 5)).get
    val res = screen.draw(Rectangle(Point(5, 4), Point(12, 10)))
    res.isSuccess shouldBe true
  }

  it should "undo last operation by Undo command" in {
    val screen0 = EmptyScreen

    val command1 = Canvas(20, 5)
    val screen1 = screen0.draw(command1).get
    screen1 shouldBe DrawingScreen(command1, getContent("emptyCanvas20x5.txt"), screen0)

    val command2 = Line(Point(5, 2), Point(5, 3))
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

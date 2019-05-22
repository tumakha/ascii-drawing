package drawing.screen

import drawing.command.{Canvas, Line, Quit, Rectangle}
import org.scalatest._

import scala.io.Source
import scala.util.Success


class ScreenSpec() extends FlatSpec with Matchers {

  "Screen" should "create canvas with specified size" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5)) shouldBe Success(getContent("emptyCanvas20x5.txt"))
    screen.draw(Canvas(3, 7)) shouldBe Success(getContent("emptyCanvas3x7.txt"))
    screen.draw(Canvas(1, 1)) shouldBe Success(getContent("emptyCanvas1x1.txt"))
    screen.draw(Canvas(0, 0)) shouldBe Success(getContent("emptyCanvas0x0.txt"))
    screen.draw(Canvas(130, 40)) shouldBe Success(getContent("emptyCanvas130x40.txt"))
  }

  it should "return Failure(exception) if height > maxHeight" in {
    val res = Screen().draw(Canvas(1, 50))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas height should be in range 0..${Screen.defaultTerminalHeight}"
  }

  it should "return Failure(exception) if width > maxWidth" in {
    val res = Screen().draw(Canvas(200, 2))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Canvas width should be in range 0..${Screen.defaultTerminalWidth}"
  }

  it should "draw horizontal line" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Line(1, 3, 7, 3)) shouldBe Success(getContent("horizLine.txt"))
  }

  it should "draw trimmed horizontal lines" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Line(0, 3, 7, 3))
    screen.draw(Line(12, 1, 30, 1)) shouldBe Success(getContent("trimmedHorizLines.txt"))
  }

  it should "return Failure(exception) if Horizontal line coordinate y is outside canvas area" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    val y = 90
    val res = screen.draw(Line(12, y, 30, y))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Coordinate y = $y is outside canvas area"
  }

  it should "draw vertical line" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Line(5, 2, 5, 3)) shouldBe Success(getContent("vertLine.txt"))
  }

  it should "draw trimmed vertical lines" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Line(5, 0, 5, 3))
    screen.draw(Line(13, 5, 13, 50)) shouldBe Success(getContent("trimmedVertLines.txt"))
  }

  it should "return Failure(exception) if Vertical line coordinate x is outside canvas area" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    val x = 200
    val res = screen.draw(Line(x, 1, x, 5))
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Coordinate x = $x is outside canvas area"
  }

  it should "draw line contains single point" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Line(12, 3, 12, 3)) shouldBe Success(getContent("singlePointLine.txt"))
  }

  it should "draw rectangle" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    screen.draw(Rectangle(5, 2, 17, 5)) shouldBe Success(getContent("rectangle.txt"))
  }

  it should "return Failure(exception) if any rectangle coordinate x is outside canvas area" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    val x = 21
    val res = screen.draw(Rectangle(5, 2, x, 5))
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual s"Coordinate x = $x is outside canvas area"
  }

  it should "return Failure(exception) if any rectangle coordinate y is outside canvas area" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))
    val y = 10
    val res = screen.draw(Rectangle(5, 4, 12, y))
    res.isSuccess shouldBe false
    res.failed.get.getMessage shouldEqual s"Coordinate y = $y is outside canvas area"
  }

  it should "not handle Quit command" in {
    val screen = Screen()
    screen.draw(Canvas(20, 5))

    val res = screen.draw(Quit)
    res.isFailure shouldBe true
    res.failed.get.getMessage shouldEqual s"Command Quit is not supported"
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

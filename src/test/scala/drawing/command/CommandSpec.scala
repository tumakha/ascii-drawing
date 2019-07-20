package drawing.command

import org.scalatest._

import scala.util.Success

/**
  * @author Yuriy Tumakha
  */
class CommandSpec extends FlatSpec with Matchers {

  "Command" should "parse Create Canvas command" in {
    Command.parse("C 20 5") shouldBe Success(Canvas(20, 5))
    Command.parse("C 132 43") shouldBe Success(Canvas(132, 43))
  }

  it should "return Exception if incorrect Create Canvas format" in {
    Command.parse("C 1 2 3").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("C 1.5 2").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("C -10 2").failed.get.getMessage shouldBe "Wrong command format"
  }

  it should "parse Draw Line command" in {
    Command.parse("L 1 3 7 3") shouldBe Success(Line(Point(1, 3), Point(7, 3)))
    Command.parse("L 7 1 7 3") shouldBe Success(Line(Point(7, 1), Point(7, 3)))

    val command = Command.parse("L 22 130 22 4")
    command shouldBe Success(Line(Point(22, 130), Point(22, 4)))

    val line = command.get.asInstanceOf[Line]
    line.horizontal shouldBe false
    line.vertical shouldBe true
  }

  it should "return Exception if incorrect Draw Line format" in {
    Command.parse("L 20 5 15 2").failed.get.getMessage shouldBe "Only horizontal or vertical lines are supported"
    Command.parse("L 1 2 3").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("L 1.5 2").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("L -10 2").failed.get.getMessage shouldBe "Wrong command format"
  }

  it should "parse Draw Rectangle command" in {
    Command.parse("R 15 2 20 5") shouldBe Success(Rectangle(Point(15, 2), Point(20, 5)))
    Command.parse("R 1 20 130 40") shouldBe Success(Rectangle(Point(1, 20), Point(130, 40)))
  }

  it should "return Exception if incorrect Draw Rectangle format" in {
    Command.parse("R 20 5 15 2").failed.get.getMessage shouldBe "Rectangle upper left corner coordinates should be specified firstly"
    Command.parse("R 1 2 3").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("R 1.5 2").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("R -10 2").failed.get.getMessage shouldBe "Wrong command format"
  }

  it should "parse Undo command" in {
    Command.parse("U") shouldBe Success(Undo)
  }

  it should "parse Quit command" in {
    Command.parse("Q") shouldBe Success(Quit)
  }

  it should "return Exception for wrong Quit format" in {
    Command.parse("Q ").failed.get.getMessage shouldBe "Wrong command format"
    Command.parse("Queue").failed.get.getMessage shouldBe "Wrong command format"
  }

}
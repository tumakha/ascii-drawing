package drawing.command

import org.scalatest._

import scala.util.Success

/**
  * @author Yuriy Tumakha
  */
class CommandParsersSpec extends FlatSpec with Matchers {

  "CommandParsers" should "parse Create Canvas command" in {
    CommandParsers.parse("C 20 5") shouldBe Success(Canvas(20, 5))
    CommandParsers.parse("C 132 43") shouldBe Success(Canvas(132, 43))
  }

  it should "return Exception if incorrect Create Canvas format" in {
    CommandParsers.parse("C 1 2 3").failed.get.getMessage shouldBe "Wrong command format. end of input expected"
    CommandParsers.parse("C 1.5 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '.' found"
    CommandParsers.parse("C -10 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '-' found"
  }

  it should "parse Draw Line command" in {
    CommandParsers.parse("L 1 3 7 3") shouldBe Success(Line(Point(1, 3), Point(7, 3)))
    CommandParsers.parse("L 7 1 7 3") shouldBe Success(Line(Point(7, 1), Point(7, 3)))

    val command = CommandParsers.parse("L 22 130 22 4")
    command shouldBe Success(Line(Point(22, 130), Point(22, 4)))

    val line = command.get.asInstanceOf[Line]
    line.horizontal shouldBe false
    line.vertical shouldBe true
  }

  it should "return Exception if incorrect Draw Line format" in {
    CommandParsers.parse("L 20 5 15 2").failed.get.getMessage shouldBe
      "requirement failed: Only horizontal or vertical lines are supported"
    CommandParsers.parse("L 1 2 3").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but end of source found"
    CommandParsers.parse("L 1.5 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '.' found"
    CommandParsers.parse("L -10 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '-' found"
  }

  it should "parse Draw Rectangle command" in {
    CommandParsers.parse("R 15 2 20 5") shouldBe Success(Rectangle(Point(15, 2), Point(20, 5)))
    CommandParsers.parse("R 1 20 130 40") shouldBe Success(Rectangle(Point(1, 20), Point(130, 40)))
  }

  it should "return Exception if incorrect Draw Rectangle format" in {
    CommandParsers.parse("R 20 5 15 2").failed.get.getMessage shouldBe
      "requirement failed: Rectangle upper left corner coordinates should be specified firstly"
    CommandParsers.parse("R 1 2 3").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but end of source found"
    CommandParsers.parse("R 1.5 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '.' found"
    CommandParsers.parse("R -10 2").failed.get.getMessage shouldBe "Wrong command format. string matching regex '\\d{1,3}' expected but '-' found"
  }

  it should "parse Undo command" in {
    CommandParsers.parse("U") shouldBe Success(Undo)
  }

  it should "parse Quit command" in {
    CommandParsers.parse("Q") shouldBe Success(Quit)
    CommandParsers.parse("Q ") shouldBe Success(Quit)
  }

  it should "return Exception for wrong Quit format" in {
    CommandParsers.parse("Queue").failed.get.getMessage shouldBe "Wrong command format. end of input expected"
  }

}
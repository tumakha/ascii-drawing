package drawing.command

import org.scalatest._

/**
  * @author Yuriy Tumakha
  */
class CommandSpec extends FlatSpec with Matchers {

  "Command" should "parse Create Canvas command" in {
    Command.parse("C 20 5") shouldBe Right(Canvas(20, 5))
    Command.parse("C 132 43") shouldBe Right(Canvas(132, 43))
  }

  it should "return Left(error) if incorrect Create Canvas format" in {
    Command.parse("C 1 2 3") shouldBe Left("Wrong command format")
    Command.parse("C 1.5 2") shouldBe Left("Wrong command format")
    Command.parse("C -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Draw Line command" in {
    Command.parse("L 1 3 7 3") shouldBe Right(Line(1, 3, 7, 3))
    Command.parse("L 7 1 7 3") shouldBe Right(Line(7, 1, 7, 3))

    val command = Command.parse("L 22 130 22 4")
    command shouldBe Right(Line(22, 130, 22, 4))

    val line = command.right.get.asInstanceOf[Line]
    line.horizontal shouldBe false
    line.vertical shouldBe true
  }

  it should "return Left(error) if incorrect Draw Line format" in {
    Command.parse("L 20 5 15 2") shouldBe Left("Only horizontal or vertical lines are supported")
    Command.parse("L 1 2 3") shouldBe Left("Wrong command format")
    Command.parse("L 1.5 2") shouldBe Left("Wrong command format")
    Command.parse("L -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Draw Rectangle command" in {
    Command.parse("R 15 2 20 5") shouldBe Right(Rectangle(15, 2, 20, 5))
    Command.parse("R 1 20 130 40") shouldBe Right(Rectangle(1, 20, 130, 40))
  }

  it should "return Left(error) if incorrect Draw Rectangle format" in {
    Command.parse("R 20 5 15 2") shouldBe Left("Rectangle upper left corner coordinates should be specified firstly")
    Command.parse("R 1 2 3") shouldBe Left("Wrong command format")
    Command.parse("R 1.5 2") shouldBe Left("Wrong command format")
    Command.parse("R -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Quit command" in {
    Command.parse("Q") shouldBe Right(Quit)
  }

  it should "return Left(error) for wrong Quit format" in {
    Command.parse("Q ") shouldBe Left("Wrong command format")
    Command.parse("Queue") shouldBe Left("Wrong command format")
  }

}
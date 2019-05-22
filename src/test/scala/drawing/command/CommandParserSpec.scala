package drawing.command

import org.scalatest._

/**
  * @author Yuriy Tumakha
  */
class CommandParserSpec extends FlatSpec with Matchers {

  "CommandParser" should "parse Create Canvas command" in {
    val commandParser = CommandParser()
    commandParser.parse("C 20 5") shouldBe Right(Canvas(20, 5))
    commandParser.parse("C 132 43") shouldBe Right(Canvas(132, 43))
  }

  it should "return Left(error) if incorrect Create Canvas format" in {
    val commandParser = CommandParser()
    commandParser.parse("C 1 2 3") shouldBe Left("Wrong command format")
    commandParser.parse("C 1.5 2") shouldBe Left("Wrong command format")
    commandParser.parse("C -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Draw Line command" in {
    val commandParser = CommandParser()
    commandParser.parse("L 1 3 7 3") shouldBe Right(Line(1, 3, 7, 3))
    commandParser.parse("L 7 1 7 3") shouldBe Right(Line(7, 1, 7, 3))

    val command = commandParser.parse("L 22 130 22 4")
    command shouldBe Right(Line(22, 130, 22, 4))

    val line = command.right.get.asInstanceOf[Line]
    line.horizontal shouldBe false
    line.vertical shouldBe true
  }

  it should "return Left(error) if incorrect Draw Line format" in {
    val commandParser = CommandParser()
    commandParser.parse("L 20 5 15 2") shouldBe Left("Only horizontal or vertical lines are supported")
    commandParser.parse("L 1 2 3") shouldBe Left("Wrong command format")
    commandParser.parse("L 1.5 2") shouldBe Left("Wrong command format")
    commandParser.parse("L -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Draw Rectangle command" in {
    val commandParser = CommandParser()
    commandParser.parse("R 15 2 20 5") shouldBe Right(Rectangle(15, 2, 20, 5))
    commandParser.parse("R 1 20 130 40") shouldBe Right(Rectangle(1, 20, 130, 40))
  }

  it should "return Left(error) if incorrect Draw Rectangle format" in {
    val commandParser = CommandParser()
    commandParser.parse("R 20 5 15 2") shouldBe Left("Rectangle upper left corner coordinates should be specified firstly")
    commandParser.parse("R 1 2 3") shouldBe Left("Wrong command format")
    commandParser.parse("R 1.5 2") shouldBe Left("Wrong command format")
    commandParser.parse("R -10 2") shouldBe Left("Wrong command format")
  }

  it should "parse Quit command" in {
    val commandParser = CommandParser()
    commandParser.parse("Q") shouldBe Right(Quit)
  }

  it should "return Left(error) for wrong Quit format" in {
    val commandParser = CommandParser()
    commandParser.parse("Q ") shouldBe Left("Wrong command format")
    commandParser.parse("Queue") shouldBe Left("Wrong command format")
  }

}
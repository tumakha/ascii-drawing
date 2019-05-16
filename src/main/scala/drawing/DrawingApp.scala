package drawing

import drawing.command.{Command, CommandParser, Quit}
import drawing.screen.Screen

import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

object DrawingApp extends App {

  val commandParser = CommandParser()
  val screen = Screen()
  var quit = false

  private def draw(command: Command): Unit =
    screen.draw(command) match {
      case Success(canvas) => println(canvas)
      case Failure(exception) => {
        printError(s"${exception.getClass.getCanonicalName}. ${exception.getMessage}")
        println(screen.printCanvas) // print previous saved canvas
      }
    }

  private def printError(error: String): Unit = println(s"ERROR: $error")

  do {
    commandParser.parse(readLine("enter command: ")) match {
      case Right(Quit()) => quit = true
      case Right(command) => draw(command)
      case Left(error) => printError(error)
    }
  } while (!quit)

}

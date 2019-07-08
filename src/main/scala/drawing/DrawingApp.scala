package drawing

import drawing.command.{Command, Quit}
import drawing.screen.Screen

import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

object DrawingApp extends App {

  val screen = Screen()

  private def draw(command: Command): Command = {
    screen.draw(command) match {
      case Success(canvas) => println(canvas)
      case Failure(exception) => {
        printError(s"${exception.getClass.getCanonicalName}. ${exception.getMessage}")
        println(screen.printCanvas) // print previous saved canvas
      }
    }
    command
  }

  private def printError(error: String): Unit = println(s"ERROR: $error")

  private def run: AnyVal =
    Command.parse(readLine("enter command: ")) match {
      case Right(command) => draw(command) == Quit
      case Left(error) => printError(error)
    }

  Stream.continually(run).find(_ == true)

}

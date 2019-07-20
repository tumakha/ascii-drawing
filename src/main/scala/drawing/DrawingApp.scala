package drawing

import drawing.command.{Command, Quit}
import drawing.screen.{EmptyScreen, Screen}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

object DrawingApp extends App {

  private def nextCommand(screen: Screen): Screen =
    Command.parse(readLine("enter command: ")).flatMap(command => screen.draw(command)) match {
      case Success(scr) => scr
      case Failure(exception) =>
        printError(s"${exception.getClass.getCanonicalName}. ${exception.getMessage}")
        screen
    }

  private def printError(error: String): Unit = println(s"ERROR: $error")

  @tailrec def run(screen: Screen): Unit = {
    println(screen.content)
    if (screen.command != Quit)
      run(nextCommand(screen))
  }

  run(EmptyScreen)

}

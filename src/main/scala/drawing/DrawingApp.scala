package drawing

import drawing.command.{Command, CommandParsers, Quit}
import drawing.screen.{EmptyScreen, Screen}

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

/**
  * @author Yuriy Tumakha
  */
object DrawingApp extends App {

  var screen: Screen = EmptyScreen

  private def draw(cmd: Try[Command]): Screen = {
    cmd flatMap screen.draw match {
      case Success(scr) => screen = scr
      case Failure(exception) => printError(s"${exception.getClass.getCanonicalName}. ${exception.getMessage}")
    }
    println(screen.content)
    screen
  }

  private def printError(error: String): Unit = println(s"ERROR: $error")

  Iterator.continually(readLine("enter command: "))
    .map(CommandParsers.parse)
    .map(draw)
    .find(_.command == Quit)

}

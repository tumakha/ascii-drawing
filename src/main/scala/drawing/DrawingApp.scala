package drawing

import drawing.command.{Command, CommandParsers, Empty, Quit}
import drawing.screen.{Drawing, EmptyScreen}

import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}

/**
  * @author Yuriy Tumakha
  */
object DrawingApp extends App {

  val drawing = new Drawing(EmptyScreen)

  private def drawScreen(cmd: Try[Command]): Command = {
    cmd flatMap drawing.draw match {
      case Success(screen) => println(screen.content)
      case Failure(exception) => printError(s"${exception.getClass.getCanonicalName}. ${exception.getMessage}")
    }
    cmd.getOrElse(Empty)
  }

  private def printError(error: String): Unit = println(Console.RED + s"ERROR: $error" + Console.RESET)

  Iterator.continually(readLine("enter command: "))
    .map(CommandParsers.parse)
    .map(drawScreen)
    .find(_ == Quit)

}

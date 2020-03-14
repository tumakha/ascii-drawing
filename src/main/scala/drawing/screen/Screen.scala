package drawing.screen

import drawing.command._
import drawing.screen.Screen.Content

/**
 * @author Yuriy Tumakha
 */
object Screen {
  // "org.jline" % "jline" % "3.11.0" can be used to get real Terminal width & height
  val defaultTerminalWidth = 130
  val defaultTerminalHeight = 40

  type Content = String
}

sealed trait Screen {
  def command: Command

  def content: Content

  def prev: Screen
}

case object EmptyScreen extends Screen {
  override def command: Command = Empty

  override def content: Content = ""

  override def prev: Screen = throw new IllegalStateException("Commands history is empty")
}

case class DrawingScreen(command: Command, content: Content, prev: Screen) extends Screen

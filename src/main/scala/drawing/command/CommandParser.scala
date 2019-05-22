package drawing.command

/**
  * @author Yuriy Tumakha
  */
case class CommandParser() {

  def parse(line: String): Either[String, Command] =
    line match {
      case Canvas.pattern(width, height) => Right(Canvas(width.toInt, height.toInt))
      case Line.pattern(x1, y1, x2, y2) if x1 == x2 || y1 == y2 =>
        Right(Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
      case Line.pattern(_, _, _, _) =>
        Left("Only horizontal or vertical lines are supported")
      case Rectangle.pattern(x1, y1, x2, y2) if x1 <= x2 && y1 <= y2 =>
        Right(Rectangle(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
      case Rectangle.pattern(_, _, _, _) =>
        Left("Rectangle upper left corner coordinates should be specified firstly")
      case "Q" => Right(Quit)
      case _ => Left("Wrong command format")
    }

}

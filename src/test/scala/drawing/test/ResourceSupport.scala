package drawing.test

import java.io.FileNotFoundException

import scala.io.Source

/**
 * @author Yuriy Tumakha
 */
trait ResourceSupport {

  def getContent(file: String): String = {
    val source = Option(getClass.getClassLoader.getResource(file))
      .map(Source.fromURL)
      .getOrElse(throw new FileNotFoundException(file))

    try {
      source.getLines().mkString("\n")
    } finally {
      source.close
    }
  }

}

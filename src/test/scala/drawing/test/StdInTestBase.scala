package drawing.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Yuriy Tumakha
  */
abstract class StdInTestBase extends FlatSpec with Matchers {

  def testStdIO(in: String, expectedOut: String)(operation: => Unit): Unit = {
    val is = new ByteArrayInputStream(in.getBytes(UTF_8))
    val os = new ByteArrayOutputStream

    Console.withIn(is) {
      Console.withOut(os) {
        operation
      }
    }

    val actual = os.toString(UTF_8.toString)
    actual shouldBe expectedOut
  }

}

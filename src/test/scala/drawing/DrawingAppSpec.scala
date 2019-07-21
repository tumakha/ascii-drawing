package drawing

import drawing.test.StdInOutTest

/**
  * @author Yuriy Tumakha
  */
class DrawingAppSpec extends StdInOutTest {

  "DrawingApp" should "handle batch of commands" in {
    val in =
      """C 20 5
        |L 1 3 7 3
        |L 7 1 7 3
        |R 15 2 20 5
        |Q
        |""".stripMargin
    val out =
      """
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||      x             |
        ||      x             |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||      x             |
        ||      x       xxxxxx|
        ||xxxxxxx       x    x|
        ||              x    x|
        ||              xxxxxx|
        |----------------------
        |enter command: """.stripMargin + "\n"

    testStdIO(in, out) {
      DrawingApp.main(null)
    }
  }

  it should "support Undo command" in {
    val in =
      """C 20 5
        |L 1 3 7 3
        |L 7 1 7 3
        |R 15 2 20 5
        |U
        |U
        |U
        |U
        |Q
        |""".stripMargin
    val out =
      """
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||      x             |
        ||      x             |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||      x             |
        ||      x       xxxxxx|
        ||xxxxxxx       x    x|
        ||              x    x|
        ||              xxxxxx|
        |----------------------
        |enter command: ----------------------
        ||      x             |
        ||      x             |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||xxxxxxx             |
        ||                    |
        ||                    |
        |----------------------
        |enter command: ----------------------
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        ||                    |
        |----------------------
        |enter command: """.stripMargin + "\nenter command: \n"

    testStdIO(in, out) {
      DrawingApp.main(null)
    }
  }

}

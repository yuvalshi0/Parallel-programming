package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  // @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  // @Test def `boxBlurKernel simple case, radius 0`(): Unit = {
  //   val src = new Img(2,2)
  //   for {
  //     i <- 0 until 2
  //     j <- 0 until 2
  //   } src(i,j) = rgba(1,1,1,1)

  //   assert(boxBlurKernel(src,0,0,1) == rgba(1,1,1,1))

  // }

  // @Test def `boxBlurKernel radius bigger than width and height`(): Unit = {
  //   val src = new Img(5,5)
  //   for {
  //     i <- 0 until 5
  //     j <- 0 until 5
  //   } src(i,j) = rgba(1,1,1,1)

  //   assert(boxBlurKernel(src,0,0,10) == rgba(1,1,1,1))
  // }

  // @Test def `boxBlurKernel radius smaller than width and height`(): Unit = {
  //   val src = new Img(5,5)
  //   for {
  //     i <- 0 until 2
  //     j <- 0 until 2
  //   } src(i,j) = rgba(1,1,1,1)

  //   for {
  //     i <- 2 until 5
  //     j <- 2 until 5
  //   } src(i,j) = rgba(2,2,2,2)

  //   assert(boxBlurKernel(src,0,0,2) == rgba(1,1,1,1) && boxBlurKernel(src,4,4,2) == rgba(2,2,2,2))
  // }

  @Test def `VerticalBoxBlur.blur with radius 2 should correctly blur the entire 4x3 image`(): Unit = {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 4)
    check(1, 0, 5)
    check(2, 0, 5)
    check(3, 0, 6)
    check(0, 1, 4)
    check(1, 1, 5)
    check(2, 1, 5)
    check(3, 1, 6)
    check(0, 2, 4)
    check(1, 2, 5)
    check(2, 2, 5)
    check(3, 2, 6)
  }
}

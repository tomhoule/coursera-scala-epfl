package scalashop

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel should return the correct value on an edge pixel") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 0, 0, 1) === 2,
      s"(boxBlurKernel(0, 0, 1) should be 2, " +
        s"but it's ${boxBlurKernel(src, 0, 0, 1)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    def check(x: Int, y: Int, expected: Int) =
      assert(dst(x, y) == expected,
        s"(destination($x, $y) should be $expected)")

    check(0, 0, 2)
    check(1, 0, 2)
    check(2, 0, 3)
    check(0, 1, 3)
    check(1, 1, 4)
    check(2, 1, 4)
    check(0, 2, 0)
    check(1, 2, 0)
    check(2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire " +
    "4x3 image") {
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

  test("Vertical parBlurRanges works as expected") {
    val w = 4
    val h = 32
    val src = new Img(w, h)
    assert(VerticalBoxBlur.parBlurRanges(src, 2) == List((0, 2), (2, 4)))
  }

  test("Vertical purBlurRanges should not produce OOB indexes") {
    val w = 5
    val h = 32
    val src = new Img(w, h)
    assert(VerticalBoxBlur.parBlurRanges(src, 2) == List((0, 2), (2, 4), (4, 5)))
  }

  test("Vertical parBlurRanges should never produce more than (width / numTasks) + 1 ranges") {
    val w = 5
    val h = 3
    val src = new Img(w, h)
    assert(VerticalBoxBlur.parBlurRanges(src, w) == List((0, 1), (1, 2), (2, 3), (3, 4), (4, 5)))
  }

  test("Horizontal parBlurRanges works as expected") {
    val w = 33
    val h = 4
    val src = new Img(w, h)
    assert(HorizontalBoxBlur.parBlurRanges(src, 2) == List((0, 2), (2, 4)))
  }

  test("Horizontal parBlurRanges should never produce more than (height / numTasks) + 1 ranges") {
    val w = 3
    val h = 37
    val src = new Img(w, h)
    assert(HorizontalBoxBlur.parBlurRanges(src, 1).length == 1)
  }

  test("Horizontal purBlurRanges should not produce OOB indexes") {
    val w = 32
    val h = 5
    val src = new Img(w, h)
    assert(HorizontalBoxBlur.parBlurRanges(src, 2) == List((0, 2), (2, 4), (4, 5)))
  }

  test("Horizontal parBlurRanges should not try to parallelize if there is only one task") {
    val w = 33
    val h = 5
    val src = new Img(w, h)
    assert(HorizontalBoxBlur.parBlurRanges(src, 1) == List((0, 5)))
  }
}

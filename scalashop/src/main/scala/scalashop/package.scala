
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val xBound = src.width - 1
    val yBound = src.height - 1
    val yStart = clamp(y - radius, 0, yBound)
    val yEnd = clamp(y + radius, 0, yBound)
    val xStart = clamp(x - radius, 0, xBound)
    val xEnd = clamp(x + radius, 0, xBound)
    var i: Int = yStart
    var j: Int = xStart
    val surface: Int = (xEnd - xStart + 1) * (yEnd - yStart + 1)
    var redSum: Int = 0
    var greenSum: Int = 0
    var blueSum: Int = 0
    var alphaSum: Int = 0

    while (i <= yEnd) {
      while (j <= xEnd) {
        redSum += red(src(j, i))
        blueSum += blue(src(j, i))
        greenSum += green(src(j, i))
        alphaSum += alpha(src(j, i))
        j += 1
      }
      j = xStart
      i += 1
    }

    rgba(redSum / surface, greenSum / surface, blueSum / surface, alphaSum / surface)
  }

}

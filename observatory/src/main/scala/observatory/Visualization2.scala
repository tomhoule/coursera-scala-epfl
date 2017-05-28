package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) +
    d10 * x * (1 - y) +
    d01 * (1 - x) * y +
    d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    def pixel(color: Color): Pixel = {
      Pixel(color.red, color.green, color.blue, 127)
    }

    val tileSize = Math.pow(2, zoom)
    val pixels: Array[Pixel] = new Array(256 * 256)
    for (row <- 0 until 256) {
      for (col <- 0 until 256) {
        val zoomedSide = Math.pow(2, zoom)
        val pixelX: Double = x + (col / zoomedSide)
        val pixelY: Double = y + (row / zoomedSide)
        val topLeft: (Int, Int) = (Math.floor(pixelX).toInt, Math.floor(pixelY).toInt)
        val bottomRight: (Int, Int) = (Math.ceil(pixelX).toInt, Math.ceil(pixelY).toInt)
        val pixelValue: Double = bilinearInterpolation(
          pixelX,
          pixelY,
          grid(topLeft._1, topLeft._2),
          grid(topLeft._1, bottomRight._2),
          grid(bottomRight._1, topLeft._2),
          grid(bottomRight._1, bottomRight._2))
        val pixelColor = Visualization.interpolateColor(colors, pixelValue)
        pixels((256 * row) + col) = pixel(pixelColor)
      }
    }

    Image(256, 256, pixels)
  }

}

package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    // The map is a square with side length mapSide
    val mapSide = Math.pow(2, zoom)
    val lonDeg = x / mapSide * 360.0 - 180.0
    val latRad = Math.atan(Math.sinh(Math.PI * (1.0 - 2.0 * y / mapSide)))
    val latDeg = latRad * 180 / Math.PI
    // val lat = Math.atan(Math.sinh(Math.PI * (1.0 - 2.0 * y / mapSide))).toDegrees
    // val lon = (x / mapSide) * 360 - 180
    Location(latDeg, lonDeg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    System.err.println(temperatures)
    System.err.println(colors)
    System.err.println(zoom)
    System.err.println(x)
    System.err.println(y)

    def pixel(color: Color): Pixel = {
      Pixel(color.red, color.green, color.blue, 127)
    }

    val side = Math.pow(2, zoom).toInt
    val pixels: Array[Pixel] = new Array(256 * 256)

    for (row <- 0 until 256) {
      for (col <- 0 until 256) {
        val pixelX = if (col == 0) x else x + col / 256
        val pixelY = if (row == 0) y else y + row / 256
        val pixelLocation = tileLocation(zoom + 8, pixelX, pixelY)
        val pixelTemperature = Visualization.predictTemperature(temperatures, pixelLocation)
        val pixelColor = Visualization.interpolateColor(colors, pixelTemperature)
        pixels((256 * row) + col) = pixel(pixelColor)
      }
    }

    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    System.err.println(yearlyData)
    for (year <- yearlyData) {
      for (zoomLevel <- 0 to 3) {
        val tilesNo: Int = Math.pow(2, zoomLevel).toInt
        for (tileX <- 0 until tilesNo) {
          for (tileY <- 0 until tilesNo) {
            generateImage(year._1, zoomLevel, tileX, tileY, year._2)
          }
        }
      }
    }
  }

}

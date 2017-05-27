package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val p: Double = 2

  val radiusOfEarth = 6367 // km

  def gpsToRadians(value: Double): Double = {
    (Math.PI/180) * value
  }

  // Great-circle distance in km
  // https://en.wikipedia.org/wiki/Great-circle_distance
  def distance(loc1: Location, loc2: Location): Double = {
    val a = Math.sin(gpsToRadians(loc1.lat)) * Math.sin(gpsToRadians(loc2.lat))
    val b = Math.cos(gpsToRadians(loc1.lat)) * Math.cos(gpsToRadians(loc2.lat)) * Math.cos(
      gpsToRadians(loc2.lon - loc1.lon))
    val centralAngle = Math.acos(a + b)

    centralAngle * radiusOfEarth
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    def w(dist: Double): Double = {
      1 / Math.pow(dist, p)
    }

    var top: Double = 0
    var bottom: Double = 0

    for (temp <- temperatures) {
      if (location == temp._1) return temp._2
      val dist = distance(temp._1, location)
      if (dist <= 1) return dist
      val wdist = w(dist)
      top += wdist * temp._2
      bottom += wdist
    }

    top / bottom
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    var previous = points.minBy(_._1)
    var next = points.maxBy(_._1)

    for (point <- points) {
      if (point._1 == value)
        return point._2
      if (point._1 <= value && point._1 > previous._1)
        previous = point
      if (point._1 >= value && point._1 < next._1)
        next = point
    }

    if (next._1 < value)
      return next._2

    if (previous._1 > value)
      return previous._2

    val red = ((previous._2.red * (next._1 - value)) + (next._2.red * (value - previous._1))) / (next._1 - previous._1)
    val green = ((previous._2.green * (next._1 - value)) + (next._2.green * (value - previous._1))) / (next._1 - previous._1)
    val blue = ((previous._2.blue * (next._1 - value)) + (next._2.blue * (value - previous._1))) / (next._1 - previous._1)

    Color(
      Math.min(255, Math.round(red.toFloat)),
      Math.min(255, Math.round(green.toFloat)),
      Math.min(255, Math.round(blue.toFloat))
    )
  }

  def coordinates(pixelCoord: Int, w: Int, h: Int): Location = {
    val row = Math.round(Math.floor(pixelCoord / w))
    val col = pixelCoord % w

    Location(90 - row, col - 180)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    def pixel(color: Color): Pixel = {
      Pixel(color.red, color.green, color.blue, 1)
    }

    val w = 360
    val h = 180
    val pixelsNumber = w * h

    val pixels: Array[Pixel] = new Array(pixelsNumber)

    for (i <- 0 until pixelsNumber) {
      pixels(i) = pixel(interpolateColor(colors, predictTemperature(temperatures, coordinates(i, w, h))))
    }

    Image(w, h, pixels)
  }

}


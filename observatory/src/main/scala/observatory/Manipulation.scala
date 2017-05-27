package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  object Grid {
    val width = 360
    val height = 180
    val origin = (0, 0)

    def gridCoordinatesToLocation(gridLat: Int, gridLon: Int): Location = {
      Location(gridLat, gridLon)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {

    // val locationOnGrid = 
    (lat, lon) =>
      Visualization.predictTemperature(temperatures, Grid.gridCoordinatesToLocation(lat, lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    (lat, lon) => {
      val grids = temperaturess.map(makeGrid(_))
      val temperaturesAtLocation: Iterable[Double] = grids.map(_(lat, lon))
      temperaturesAtLocation.foldLeft(0.0)((acc: Double, item: Double) => acc + item) / temperaturesAtLocation.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    (lat, lon) => {
      val loc = Location(lat, lon)
      val observed = temperatures.find(loc == _._1).map(_._2).get
      val normal = normals(lat, lon)
      observed - normal
    }
  }


}


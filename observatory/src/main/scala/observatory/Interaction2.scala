package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {

  val devColorScale = Seq((0.0, Color(255, 255, 255)), (1.0, Color(0, 0, 0)))
  val tempColorScale = Seq((1.0, Color(255, 255, 255)), (0.0, Color(0, 0, 0)))

  def devLayer = Layer(LayerName.Deviations, Interaction2.devColorScale, 1975 to 2015)
  def tempLayer = Layer(LayerName.Temperatures, Interaction2.tempColorScale, 1975 to 2015)

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = {
    Seq(devLayer, tempLayer)
  }

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = {
    Signal(selectedLayer().bounds)
  }

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Int]): Signal[Int] = {
    val bounds = selectedLayer().bounds
    val v = sliderValue()
    if (v < bounds.start) {
      Signal(bounds.start)
    } else if (v > bounds.end) {
      Signal(bounds.end)
    } else {
      Signal(v)
    }
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal("")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Int]): Signal[String] = {
    Signal("")
  }

}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Double, Color)], bounds: Range)

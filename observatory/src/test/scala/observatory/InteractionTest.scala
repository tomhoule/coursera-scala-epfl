package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap
import com.sksamuel.scrimage.{RGBColor, Image, Pixel}

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("tileLocation works") {
    assert(Interaction.tileLocation(0, 0, 0).lat === 85.05112877980659)
  }

  test("tile") {
    val temperatures = List(
      (new Location(45.0 , -90.0), 20.0),
      (new Location(45.0 , 90.0 ), 0.0),
      (new Location(0.0  , 0.0  ), 10.0),
      (new Location(-45.0, -90.0), 0.0),
      (new Location(-45.0, 90.0 ), 20.0)
    )

    val colors = List(
      (0.0  , Color(255, 0  , 0)),
      (10.0 , Color(0  , 255, 0)),
      (20.0 , Color(0  , 0  , 255))
    )

    val temperatures2 = List((Location(45.0,-90.0),10.0), (Location(-45.0,0.0),20.0))
    val colors2 = List((10.0,Color(255,0,0)), (20.0,Color(0,0,255)))
    val result2 = Interaction.tile(temperatures2, colors2, 0, 0, 0)
    // assert(result2.pixel(0, 12).toColor === RGBColor(255, 0, 0))
  }
}

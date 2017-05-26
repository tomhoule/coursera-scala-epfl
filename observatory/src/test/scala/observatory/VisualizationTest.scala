package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("distance between Gap and Marseille is right") {
    val gap = Location(44.4465, 6.5176)
    val marseille = Location(43.1821, 5.809)
    assert(Visualization.distance(gap, marseille) > 150)
    assert(Visualization.distance(gap, marseille) < 155)
  }

  test("distance between Berlin and Paris is approximately right") {
    val berlin = Location(52.51469, 13.43291)
    val paris = Location(48.870991, 2.384650)
    assert(Visualization.distance(berlin, paris) > 870)
    assert(Visualization.distance(berlin, paris) < 880)
  }

  test("distance between Murmansk and London is approximately right") {
    val london = Location(51.51200, -0.121900)
    val murmansk = Location(68.9712, 33.055)
    assert(Visualization.distance(london, murmansk) > 2600)
    assert(Visualization.distance(murmansk, london) < 2700)
  }

  test("predictTemperature does not crash with small input") {
    val input = Set().toIterable
    Visualization.predictTemperature(input, Location(1, -1))
  }

  test("predictTemperature gives a reasonable answer") {
    val input = Set(
      (Location(51.51200, -0.121900), 20.0),
      (Location(51.51200, -0.121900), 23.0),
      (Location(51.51200, -0.121900), 18.0),
      (Location(68.9712, 33.055), -44.9)
    ).toIterable
    assert(Visualization.predictTemperature(input, Location(51.0, 2.1)) > 20)
    assert(Visualization.predictTemperature(input, Location(51.0, 2.1)) < 21)
  }

  test("interpolateColor predicts the correct color") {
    val points = Set(
      (-55.0, Color(255, 0, 255)),
      (55.0, Color(0, 255, 0))
    ).toIterable
    assert(Visualization.interpolateColor(points, 0) === Color(128, 128, 128))
  }

  test("interpolateColor predicts the correct color (more complex)") {
    val points = Set(
      (-55.0, Color(255, 0, 255)),
      (-10.0, Color(200, 200, 200)),
      (55.0, Color(0, 255, 0))
    ).toIterable
    assert(Visualization.interpolateColor(points, 12) === Color(132, 219, 132))
  }

  test("coordinates seems to work") {
    val topLeft = 0
    val end = (360 * 180)
    val origin = (360 * 90) + 180

    assert(Visualization.coordinates(topLeft, 360, 180) === Location(90, -180))
    assert(Visualization.coordinates(origin, 360, 180) === Location(0, 0))
    assert(Visualization.coordinates(end, 360, 180) === Location(-90, -180))
  }

  test("interpolateColor handles extreme values correctly") {
    assert(Visualization.interpolateColor(
      List((-1.0, Color(255,0,0)), (1.0, Color(0, 0, 255))),
      -1.0
    ) === Color(255, 0, 0))

    assert(Visualization.interpolateColor(
      List((-100.0,Color(255,0,0)), (-19.793294989955413,Color(0,0,255))),
      -110
    ) === Color(255, 0, 0))
  }

  test("predictTemperature does not mix up distances") {
    val input = Set(
      (Location(51.51200, -0.121900), 20.0),
      (Location(31.51200, 22.12), 10.0)
    ).toIterable
    assert(Visualization.predictTemperature(input, Location(51.5, 5.2)) > 19)
    assert(Visualization.predictTemperature(input, Location(51.5, 5.2)) < 20)
  }
}

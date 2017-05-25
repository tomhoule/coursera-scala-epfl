package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("locationYearlyAverageRecords works") {
    val loc1 = Location(1, 1)
    val loc2 = Location(0, 3)
    val result = Extraction.locationYearlyAverageRecords(Seq(
      (LocalDate.of(2017, 1, 31), loc1, 10),
      (LocalDate.of(2017, 1, 29), loc2, 30),
      (LocalDate.of(2017, 1, 28), loc1, 30),
      (LocalDate.of(2017, 1, 30), loc2, 22),
      (LocalDate.of(2017, 4, 30), loc2, 26),
      (LocalDate.of(2017, 7, 30), loc2, 10)
    ))
    assert(result === Seq((loc1, 20), (loc2, 22)))
  }

  test("localTemperatures works") {
    Extraction.locateTemperatures(2003, "/stations.csv", "/2003.csv")
  }

  test("localTemperatures reports temperatures for stations without wban") {
    assert(
      Extraction.locateTemperatures(2003, "/tests_stations.csv", "/tests_temperatures.csv").toSet ===
        Set(
          (LocalDate.of(2003, 12, 30), Location(1, -1), 10),
          (LocalDate.of(2003, 12, 17), Location(2, -1), 10),
          (LocalDate.of(2003, 12, 22), Location(1, -2), 10),
          (LocalDate.of(2003, 12, 23), Location(1, -3), 10)
        )
    )
  }

  test("it complies with the assignment example") {
    assert(
      Extraction.locateTemperatures(2015, "/tests_stations_2.csv", "/tests_temperatures_2.csv").toSet ===
        Set(
          (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
          (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
          (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
        ))
  }

//   test("localTemperatures reports temperatures for stations without stn") {
//   }

//   test("localTemperatures reports temperatures for stations with both wban and stn") {
  // }
}

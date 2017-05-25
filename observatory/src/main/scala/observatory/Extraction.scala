package observatory

import java.lang.ClassLoader
import java.time.LocalDate
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsRaw = Source.fromInputStream(getClass.getResourceAsStream(stationsFile))
    val temperaturesRaw = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))
    // STN, WBAN, lat, lon
    val locations: Iterable[(String, String, Location)] = stationsRaw.getLines().flatMap(line => {
      val cols: Array[String] = line.split(",", -1)
      val tentative = (cols(0), cols(1), cols(2), cols(3))
      tentative match {
        case (_, _, "", "") => None
        case ("", "", _, _) => None
        case (stn, wban, lat, lon) => Some((stn, wban, Location(lat.toDouble, lon.toDouble)))
      }
    }).toIterable
    // STN, WBAN, Date, Temperature
    val temperatures: Iterable[(String, String, LocalDate, Double)] = temperaturesRaw.getLines().flatMap(line => {
      val cols = line.split(",", -1)
      val temperature = {
        val raw = cols(4).toDouble
        if (raw == 9999.9) None else Some(raw)
          .map(temp => ((temp - 32) * 5) / 9)
          .map(temp => Math.round(temp*100.0)/100.0)

      }
      val tentative = (cols(0), cols(1), LocalDate.of(year, cols(2).toInt, cols(3).toInt), temperature)
      tentative match {
        case ("", "", _, _) => None
        case (_, _, _, None) => None
        case (stn, wban, date, Some(temperature)) => Some((stn, wban, date, temperature))
      }
    }).toIterable

    temperatures.flatMap(temperature => {
      val target: (String, String) = (temperature._1, temperature._2)
      if (temperature._1.isEmpty || temperature._1 == null)
          locations
            .find(loc => temperature._2 == loc._2 && !loc._2.isEmpty && loc._1.isEmpty)
            .map(loc => (
              temperature._3,
              loc._3,
              temperature._4
            ))
      else if (temperature._2.isEmpty || temperature._1 == null) {
          locations
            .find(loc => temperature._1 == loc._1 && !loc._1.isEmpty && loc._2.isEmpty)
            .map(loc => (
              temperature._3,
              loc._3,
              temperature._4
            ))
      } else
          locations
            .find(loc => (temperature._1 == loc._1) && (temperature._2 == loc._2))
            .map(loc => (
              temperature._3,
              loc._3,
              temperature._4
            ))
    }).toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(record => record._2)
      .values
      .map(iter => (iter.head._2, iter.map(_._3).reduce((acc: Double, temp: Double) => acc + temp) / iter.size))
      .toIterable
  }

}

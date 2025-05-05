package io

import java.nio.file.{Files, Path}
import scala.util.{Using, Try}
import com.github.tototoshi.csv.*
import models.*

object CsvService:


  private enum Col { case Time, MW, Kind }
  private val header = Col.values.map(_.toString)


  def save[T <: EnergyRecord](
      recs:  Seq[T],
      file:  Path,
      append:Boolean = false
  ): Either[Throwable, Unit] =
    Try {
      val needHeader = !append || !Files.exists(file)
      val w = CSVWriter.open(file.toFile, append = append)
      if needHeader then w.writeRow(header)
      recs.foreach { r =>
        val kind = r match
          case _: SolarRecord => "solar"
          case _: WindRecord  => "wind"
          case _: HydroRecord => "hydro"
        w.writeRow(Seq(r.timestamp.toString, r.kWh.toString, kind))
      }
      w.close()
    }.toEither

  def load(file: Path): Either[Throwable, Seq[EnergyRecord]] =
    Try {
      val r   = CSVReader.open(file.toFile)
      val all = r.allWithHeaders()
      r.close()
      all.flatMap { row =>
        for
          ts  <- row.get(Col.Time.toString)
          kwS <- row.get(Col.MW.toString)
          kwh <- Try(kwS.toDouble).toOption
          rec <- row.getOrElse(Col.Kind.toString, "") match
                   case "solar" => Some(SolarRecord(java.time.LocalDateTime.parse(ts), kwh))
                   case "wind"  => Some(WindRecord (java.time.LocalDateTime.parse(ts), kwh))
                   case "hydro" => Some(HydroRecord(java.time.LocalDateTime.parse(ts), kwh))
                   case _       => None
        yield rec
      }
    }.toEither



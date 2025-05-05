// Group: Tyloo
// Members: Shaolin Liao, Gongze Li, Jikang Guo


package models

import java.time.*

sealed trait EnergyRecord:
  def timestamp: LocalDateTime
  def kWh: Double

final case class SolarRecord(timestamp: LocalDateTime, kWh: Double)  extends EnergyRecord
final case class WindRecord (timestamp: LocalDateTime, kWh: Double)  extends EnergyRecord
final case class HydroRecord(timestamp: LocalDateTime, kWh: Double)  extends EnergyRecord

object EnergyRecord:
  def onDate[T <: EnergyRecord](records: Seq[T], date: LocalDate): Seq[T] =
    records.filter(_.timestamp.toLocalDate == date)

  def mean(xs: Seq[Double]): Double =
    if xs.isEmpty then 0.0 else xs.foldLeft(0.0)(_ + _) / xs.size

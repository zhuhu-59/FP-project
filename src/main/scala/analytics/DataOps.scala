package analytics

import models.*
import java.time.*
import java.time.format.DateTimeFormatter
import java.time.temporal.WeekFields
import scala.util.Try

object DataOps:

  enum PeriodKind { case Hour, Day, Week, Month }

  case class AggRow(label: String,
                    kind: String,
                    sum: Double,
                    mean: Double,
                    median: Double,
                    mode: Option[Double],
                    range: Double,
                    midRange: Double,
                    count: Int)

  private val iso      = WeekFields.ISO
  private val hourFmt  = DateTimeFormatter.ofPattern("dd/MM/yyyy HH")
  private val dayFmt   = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  def aggregate(recs: Seq[EnergyRecord], k: PeriodKind): Seq[AggRow] =
    val grouped = k match
      case PeriodKind.Hour =>
        recs.groupBy(r => r.timestamp.format(hourFmt))
      case PeriodKind.Day  =>
        recs.groupBy(r => r.timestamp.format(dayFmt))
      case PeriodKind.Week =>
        recs.groupBy { r =>
          val y = r.timestamp.getYear
          val w = r.timestamp.get(iso.weekOfWeekBasedYear)
          f"$y-W$w%02d"
        }
      case PeriodKind.Month =>
        recs.groupBy { r =>
          val y = r.timestamp.getYear
          val m = r.timestamp.getMonthValue
          f"$m%02d/$y"
        }

    grouped.toSeq.sortBy(_._1).map { (lab, rows) =>
      val xs     = rows.map(_.kWh)
      val sumVal = xs.sum
      val kind   =
        val distinct = rows.map(_.getClass.getSimpleName).distinct
        if distinct.size == 1 then distinct.head.dropRight(6).toLowerCase  
        else "mixed"

      AggRow(
        label     = lab,
        kind      = kind,
        sum       = sumVal,
        mean      = Stats.mean(xs),
        median    = Stats.median(xs),
        mode      = Stats.mode(xs),
        range     = Stats.range(xs),
        midRange  = Stats.midRange(xs),
        count     = xs.size
      )
    }

  def sortByKWh(recs: Seq[EnergyRecord], asc: Boolean): Seq[EnergyRecord] =
    val ord = recs.sortBy(_.kWh)
    if asc then ord else ord.reverse

  def search(recs: Seq[EnergyRecord], ts: LocalDateTime): Option[EnergyRecord] =
    recs.find(_.timestamp == ts)

  def parseKind(s: String): Option[PeriodKind] = s.toLowerCase match
    case "hour" | "hourly"   => Some(PeriodKind.Hour)
    case "day"  | "daily"    => Some(PeriodKind.Day)
    case "week" | "weekly"   => Some(PeriodKind.Week)
    case "month"| "monthly"  => Some(PeriodKind.Month)
    case _                   => None

  def parseTimestamp(s: String): Option[LocalDateTime] =
    val fmt = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
    Try(LocalDateTime.parse(s, fmt)).toOption



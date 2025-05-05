// Group: Tyloo
// Members: Shaolin Liao, Gongze Li, Jikang Guo

package plot

import models.*
import java.awt.{BasicStroke, Color}
import java.io.File
import java.text.SimpleDateFormat
import java.time.ZoneId
import java.time.temporal.ChronoUnit
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.time.{Second, TimeSeries, TimeSeriesCollection}

/** Generates a PNG time‑series chart using JFreeChart */
object PlotService:

  def savePng(recs: Seq[EnergyRecord], out: File, title: String): File =
    val series = TimeSeries("MW")
    recs.foreach { r =>
      val zdt = r.timestamp.atZone(ZoneId.of("Europe/Helsinki"))
      series.addOrUpdate(new Second(
        zdt.getSecond,
        zdt.getMinute,
        zdt.getHour,
        zdt.getDayOfMonth,
        zdt.getMonthValue,
        zdt.getYear
      ), r.kWh)
    }

    val dataset = TimeSeriesCollection()
    dataset.addSeries(series)

    val chart = ChartFactory.createTimeSeriesChart(
      title,
      "time",
      "MW",
      dataset,
      false,
      false,
      false
    )

    val plot  = chart.getXYPlot
    val rend  = XYLineAndShapeRenderer(true, false)
    rend.setSeriesPaint(0, Color(0, 102, 204))
    rend.setSeriesStroke(0, new BasicStroke(1.6f))
    plot.setRenderer(rend)

    val axis = plot.getDomainAxis.asInstanceOf[DateAxis]
    axis.setAutoRange(true)

    val timestamps = recs.map(_.timestamp)
    val first = timestamps.min
    val last  = timestamps.max
    val totalDays = ChronoUnit.DAYS.between(first.toLocalDate, last.toLocalDate) + 1

    val dateFormat =
      if totalDays <= 1 then new SimpleDateFormat("HH:mm")
      else if totalDays <= 31 then new SimpleDateFormat("MM-dd")
      else new SimpleDateFormat("yyyy-MM")

    axis.setDateFormatOverride(dateFormat)

    ChartUtils.saveChartAsPNG(out, chart, 1280, 480)
    out


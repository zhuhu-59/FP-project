// Group: Tyloo
// Members: Shaolin Liao, Gongze Li, Jikang Guo

import api.FingridClient
import io.CsvService
import analytics.{Stats, DataOps}
import alarm.AlarmService
import models.*
import com.github.tototoshi.csv.{CSVWriter, defaultCSVFormat}
import java.nio.file.{Files, Paths}
import java.time.*
import java.time.format.DateTimeFormatter
import scala.util.Try
import plot.PlotService 

@main def runCLI(): Unit =
  val tsFmt  = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
  val fiZone = ZoneId.of("Europe/Helsinki")

  println("== Renewable Energy Plant System CLI Command Menu ==")

  def show(r: EnergyRecord): Unit =
    val kind = r match
      case _: SolarRecord => "solar"
      case _: WindRecord  => "wind"
      case _: HydroRecord => "hydro"
    println(f"${r.timestamp}%-20s ${r.kWh}%.2f $kind")

  def savePull(tag: String, start: LocalDateTime,
               res: Either[Throwable, Seq[EnergyRecord]]): Unit =
    res.fold(
      err => println(s"API error: $err"),
      data => {
        val p = Paths.get(s"${tag}_${start.toLocalDate}.csv")
        CsvService.save(data, p, append = true)
        println(s"Pulled ${data.size} pts -> $p")
      })

  /* ---------------- CLI ---------------- */
  Iterator.continually(scala.io.StdIn.readLine("> "))
    .takeWhile(_ != "quit")
    .foreach {

      /* ===== fetch <type> <start dd/MM/yyyy HH:mm> <end dd/MM/yyyy HH:mm> ===== */
      case cmd if cmd.startsWith("fetch") =>
        cmd.trim.split("\\s+", 6).toList match
          case _ :: kind :: stD :: stT :: enD :: enT :: Nil =>
            val stOpt = Try(LocalDateTime.parse(s"$stD $stT", tsFmt)).toOption
            val enOpt = Try(LocalDateTime.parse(s"$enD $enT", tsFmt)).toOption
            (stOpt, enOpt) match
              case (Some(st), Some(en)) if !en.isBefore(st) =>
                val endAdj = List(en, LocalDateTime.now(fiZone)).min
                kind match
                  case "solar" => savePull("solar", st, FingridClient.solar(st, endAdj))
                  case "wind"  => savePull("wind",  st, FingridClient.wind (st, endAdj))
                  case "hydro" => savePull("hydro", st, FingridClient.hydro(st, endAdj))
                  case "all" =>
                    savePull("solar", st, FingridClient.solar(st, endAdj)); Thread.sleep(3000)
                    savePull("wind",  st, FingridClient.wind (st, endAdj)); Thread.sleep(3000)
                    savePull("hydro", st, FingridClient.hydro(st, endAdj))
                  case _ => println("Unknown type (solar|wind|hydro|all).")
              case _ =>
                println("Invalid datetime. Example: fetch solar 04/05/2025 00:00 05/05/2025 13:00")
          case _ =>
            println("Usage: fetch <solar|wind|hydro|all> <dd/MM/yyyy HH:mm> <dd/MM/yyyy HH:mm>")

      /* ===== stats <file.csv> ===== */
      case cmd if cmd.startsWith("stats") =>
        val file = Paths.get(cmd.drop("stats".length).trim)
        CsvService.load(file) match
          case Right(recs) if recs.nonEmpty =>
            val xs = recs.map(_.kWh)
            println("Statistical results:")
            println(f"Min:        ${xs.min}%.2f MW")
            println(f"Midrange:   ${Stats.midRange(xs)}%.2f MW")
            println(f"Max:        ${xs.max}%.2f MW")
            println(f"Data size:  ${xs.size}")
            println(f"Mode:       ${Stats.mode(xs).fold("N/A")(m => f"$m%.2f MW")}")
            println(f"Average:    ${Stats.mean(xs)}%.2f MW")
            println(f"Median:     ${Stats.median(xs)}%.2f MW")
            println(f"Range:      ${Stats.range(xs)}%.2f MW")
          case Right(_)   => println("CSV file contains no data.")
          case Left(err)  => println(s"CSV error: ${err.getMessage}")


      /* ===== filter <in.csv> <hour|day|week|month> <out.csv> ===== */
      case cmd if cmd.startsWith("filter") =>
        cmd.trim.split("\\s+", 4).toList match
          case _ :: inF :: kindS :: outF :: Nil =>
            (CsvService.load(Paths.get(inF)), DataOps.parseKind(kindS)) match
              case (Right(recs), Some(pk)) =>
                val rows = DataOps.aggregate(recs, pk)
                val outP = Paths.get(outF)
                val w = CSVWriter.open(outP.toFile)(defaultCSVFormat)
                w.writeRow(Seq("period","kind","sum","mean","median","mode",
                               "range","midRange","count"))
                rows.foreach { r =>
                  w.writeRow(Seq(r.label,r.kind,
                                 f"${r.sum}%.2f",f"${r.mean}%.2f",f"${r.median}%.2f",
                                 r.mode.fold("")(m => f"$m%.2f"),
                                 f"${r.range}%.2f",f"${r.midRange}%.2f",r.count))
                }
                w.close()
                println(s"Wrote ${rows.size} rows -> $outP")
              case (Left(e), _) => println(s"CSV error: ${e.getMessage}")
              case _            => println("Bad kind (hour|day|week|month)")
          case _ =>
            println("Usage: filter <in.csv> <hour|day|week|month> <out.csv>")

      /* ===== sort <file> asc|desc ===== */
      case cmd if cmd.startsWith("sort") =>
        cmd.trim.split("\\s+", 3).toList match
          case _ :: fileS :: ord :: Nil =>
            val orderLower = ord.toLowerCase
            if orderLower != "asc" && orderLower != "desc" then
              println("Order must be asc or desc"); ()
            else
              CsvService.load(Paths.get(fileS)) match
                case Right(recs) =>
                  val asc   = orderLower == "asc"
                  val sorted= DataOps.sortByKWh(recs, asc)
                  val inPath = Paths.get(fileS)
                  val name   = inPath.getFileName.toString
                  val idx    = name.lastIndexOf('.')
                  val base   = if idx > 0 then name.substring(0, idx) else name
                  val outName= s"${base}_sort_${orderLower}.csv"
                  val outP   = Option(inPath.getParent).fold(Paths.get(outName))(_.resolve(outName))

                  CsvService.save(sorted, outP, append = false)
                  println(s"Sorted data written -> $outP")
                case Left(e) => println(s"CSV error: ${e.getMessage}")
          case _ => println("Usage: sort <file.csv> asc|desc")

      /* ===== search <file> <dd/MM/yyyy HH:mm> ===== */
      case cmd if cmd.startsWith("search") =>
        cmd.trim.split("\\s+", 3).toList match
          case _ :: fileS :: tsS :: Nil =>
            (CsvService.load(Paths.get(fileS)), DataOps.parseTimestamp(tsS)) match
              case (Right(recs), Some(ts)) =>
                DataOps.search(recs, ts) match
                  case Some(r) => show(r)
                  case None    => println("No record at that timestamp.")
              case (Left(e), _) => println(s"CSV error: ${e.getMessage}")
              case (_, None)    => println("Timestamp format: dd/MM/yyyy HH:mm")
          case _ => println("Usage: search <file.csv> <dd/MM/yyyy HH:mm>")

            /* ===== current <solar|wind|hydro> ===== */
      case cmd if cmd.startsWith("current") =>
        cmd.trim.split("\\s+", 2).toList match
          case _ :: kind :: Nil =>
            val res = kind match
              case "solar" => FingridClient.currentSolar()
              case "wind"  => FingridClient.currentWind()
              case "hydro" => FingridClient.currentHydro()
              case _       => Left(new Exception("Unknown type"))
            res match
              case Right(v) =>
                println(f"Current $kind output: $v%.2f MW")
                AlarmService.lowAlert(kind, v).foreach(println)
                print(s"Modify $kind threshold? (y/n) > ")
                scala.io.StdIn.readLine().trim.toLowerCase match
                  case "y" | "yes" =>
                    print(s"Enter new threshold MW (current ${AlarmService.get(kind)}): ")
                    scala.io.StdIn.readLine().toDoubleOption match
                      case Some(nv) =>
                        AlarmService.update(kind, nv)
                        println(s"Threshold updated to $nv MW.")
                      case None => println("Not a number, threshold unchanged.")
                  case _ => ()
              case Left(e) => println(s"API error: $e")
          case _ =>
            println("Usage: current <solar|wind|hydro>")
        
            /* ===== plot <file.csv> ===== */
      case cmd if cmd.startsWith("plot") =>
        cmd.trim.split("\\s+", 2).toList match
          case _ :: fileS :: Nil =>
            CsvService.load(Paths.get(fileS)) match
              case Right(recs) if recs.nonEmpty =>
                val in  = Paths.get(fileS)
                val idx = in.getFileName.toString.lastIndexOf('.')
                val base= if idx > 0 then in.getFileName.toString.substring(0, idx) else in.getFileName.toString
                val out = in.resolveSibling(s"${base}_plot.png").toFile
                PlotService.savePng(recs, out, base.replace('_', ' '))
                println(s"Chart written → $out")
              case Right(_) => println("CSV is empty.")
              case Left(e)  => println(s"CSV error: ${e.getMessage}")
          case _ =>
            println("Usage: plot <file.csv>")

      /* help */
      case "help" | "" =>
        println(
          """commands:
            |  Data Fetching
            |   fetch  <type> <start dd/MM/yyyy HH:mm> <end dd/MM/yyyy HH:mm>
            |           - Download data for solar, wind, or hydro
            |
            |  Current Status
            |   current <solar|wind|hydro>
            |           - Show latest MW reading and threshold warnings
            |
            |  Data Analysis
            |   stats   <file.csv>
            |          - Show mean, median, mode, range, midrange
            |   filter  <in.csv> <hour|day|week|month> <out.csv>
            |          - Group data by time period and export
            |   sort    <file.csv> asc|desc
            |           - Sort rows by timestamp
            |   search  <file.csv> <dd/MM/yyyy HH:mm>
            |           - Lookup row by exact timestamp
            |
            |  Chart
            |   plot    <file.csv>
            |           - Generate PNG time-series chart
            |
            |  Exit
            |   quit
            |""".stripMargin)

      /* default */
      case other =>
        println(s"Unknown cmd: $other  (type 'help')")


    }



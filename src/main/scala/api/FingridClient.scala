// Group: Tyloo
// Members: Shaolin Liao, Gongze Li, Jikang Guo

package api

import sttp.client4.*
import sttp.client4.circe.*
import io.circe.*, io.circe.generic.semiauto.*
import java.time.*, java.time.format.DateTimeFormatter
import models.*
import io.github.cdimascio.dotenv.Dotenv
import sttp.client4.HttpError

object FingridClient:

  /* ---------- API‑key ---------- */
  private val apiKey =
    sys.env.get("FINGRID_API_KEY")
      .orElse(Option(Dotenv.load().get("FINGRID_API_KEY")))
      .getOrElse(throw new Exception("FINGRID_API_KEY missing"))

  private val backend = DefaultSyncBackend()

  /* ---------- JSON ---------- */
  final case class Point(value: Double, startTime: String)
  final case class Wrapper(data: Vector[Point])
  given Decoder[Point]   = deriveDecoder
  given Decoder[Wrapper] = deriveDecoder

  private val fiZone = ZoneId.of("Europe/Helsinki")
  private inline def fi2Utc(l: LocalDateTime) = l.atZone(fiZone).withZoneSameInstant(ZoneOffset.UTC)
  private inline def utc2Fi(s: String) = ZonedDateTime.parse(s).withZoneSameInstant(fiZone).toLocalDateTime

  private def fetchRange(
      id: Int,
      startFi: LocalDateTime,
      endFi:   LocalDateTime
  ): Either[Throwable, Vector[Point]] =
    val stUtc = fi2Utc(startFi).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "Z"
    val etUtc = fi2Utc(endFi  ).format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "Z"

    val pageSize   = 2000 
    val maxRetries = 5

    var page   = 1
    var acc    = Vector.empty[Point]
    var done   = false
    var err    : Option[Throwable] = None

    while !done && err.isEmpty do
      var retries = 0
      var pageFetched = false
      while !pageFetched && retries <= maxRetries && err.isEmpty do
        val url =
          uri"https://data.fingrid.fi/api/datasets/$id/data?startTime=$stUtc&endTime=$etUtc&pageSize=$pageSize&page=$page"

        basicRequest
          .header("x-api-key", apiKey)
          .get(url)
          .response(asJson[Wrapper])
          .send(backend)
          .body match
            case Right(w) =>
              acc ++= w.data
              pageFetched = true
              if w.data.size < pageSize then done = true else page += 1

            case Left(httpErr: HttpError[String]) if httpErr.statusCode.code == 429 =>
              val waitSec =
                """(\d+)\s*seconds""".r
                  .findFirstMatchIn(httpErr.body)
                  .flatMap(m => m.group(1).toIntOption)
                  .getOrElse(2)
              retries += 1
              if retries > maxRetries then err = Some(httpErr)
              else
                println(s"[warn] 429 received, retry #$retries in $waitSec s …")
                Thread.sleep(waitSec * 1000L)

            case Left(e) =>
              err = Some(e)

    err.toLeft(acc.reverse)



  private inline def mapRec[T <: EnergyRecord](
      raw: Vector[Point], ctor: (LocalDateTime, Double) => T
  ) = raw.map(p => ctor(utc2Fi(p.startTime), p.value))

  /* Convenience wrappers returning domain records */
  def solar(st: LocalDateTime, en: LocalDateTime) =
    fetchRange(248, st, en).map(mapRec(_, SolarRecord.apply))

  def wind (st: LocalDateTime, en: LocalDateTime) =
    fetchRange(181, st, en).map(mapRec(_, WindRecord.apply))

  def hydro(st: LocalDateTime, en: LocalDateTime) =
    fetchRange(191, st, en).map(mapRec(_, HydroRecord.apply))


  private def latest(id: Int): Either[Throwable, Double] =
    val now   = LocalDateTime.now(ZoneId.of("Europe/Helsinki"))
    val start = now.minusHours(1)
    fetchRange(id, start, now).map(_.last.value)

  def currentSolar(): Either[Throwable, Double] = latest(248)
  def currentWind (): Either[Throwable, Double] = latest(181)
  def currentHydro(): Either[Throwable, Double] = latest(191)





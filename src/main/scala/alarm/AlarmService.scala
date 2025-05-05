package alarm

object AlarmService:

  private var thresholds: Map[String, Double] = Map(
    "solar" -> 90.0,
    "wind"  -> 300.0,
    "hydro" -> 500.0
  )

  def get(kind: String): Double = thresholds(kind)

  def update(kind: String, v: Double): Unit =
    thresholds = thresholds.updated(kind, v)

  def lowAlert(kind: String, value: Double): Option[String] =
    val th = thresholds(kind)
    if value < th then Some(s" $kind output low: ${value} MW (threshold $th MW)")
    else None


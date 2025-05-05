package analytics

import scala.annotation.tailrec

object Stats:

  @tailrec
  private def sumAndCount(xs: List[Double], accSum: Double, accCnt: Int): (Double, Int) =
    xs match
      case Nil      => (accSum, accCnt)
      case h :: tail => sumAndCount(tail, accSum + h, accCnt + 1)

  def mean(xs: Seq[Double]): Double =
    val (s, n) = sumAndCount(xs.toList, 0.0, 0)
    if n == 0 then 0.0 else s / n

  def meanE(xs: Seq[Double]): Either[String, Double] =
    val (s, n) = sumAndCount(xs.toList, 0.0, 0)
    if n == 0 then Left("Empty data") else Right(s / n)

  def median(xs: Seq[Double]): Double =
    if xs.isEmpty then 0.0
    else
      val sorted = xs.sorted
      val n      = sorted.size
      if n % 2 == 1 then sorted(n / 2)
      else
        val (a, b) = (sorted(n / 2 - 1), sorted(n / 2))
        (a + b) / 2.0

  def mode(xs: Seq[Double]): Option[Double] =
    xs.groupBy(identity).view.mapValues(_.size).toSeq
      .maxByOption(_._2).map(_._1)

  def range(xs: Seq[Double]): Double =
    if xs.isEmpty then 0.0 else xs.max - xs.min

  def midRange(xs: Seq[Double]): Double =
    if xs.isEmpty then 0.0 else (xs.max + xs.min) / 2


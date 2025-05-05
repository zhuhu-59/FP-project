// Group: Tyloo
// Members: Shaolin Liao, Gongze Li, Jikang Guo

package analytics

import org.scalatest.funsuite.AnyFunSuite

/* Simple munit unit tests for Stats helpers */
class StatsSpec extends AnyFunSuite:
  private val xs = Seq(10.0, 20.0, 10.0, 40.0)

  test("mean")     { assert(Stats.mean(xs)     == 20.0) }
  test("median")   { assert(Stats.median(xs)   == 15.0) }
  test("mode")     { assert(Stats.mode(xs)     contains 10.0) }
  test("range")    { assert(Stats.range(xs)    == 30.0) }
  test("midRange") { assert(Stats.midRange(xs) == 25.0) }
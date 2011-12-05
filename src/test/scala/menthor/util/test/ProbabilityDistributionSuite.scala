package menthor.util.test

import org.scalatest.FunSuite

import scalala.library.Numerics._
import menthor.util.ProbabilityDistribution

class ProbabilityDistributionSuite extends FunSuite {
  val distr = new ProbabilityDistribution(Map("a" -> 2, "b" -> 3))

  test("logSum") {
    assert(logSum(Math.log(100), Math.log(200)) === Math.log(300))
    assert(logSum(List(Math.log(100), Math.log(200))) === Math.log(300))
  }

  test("normalizeLogProbabilty") {
    assert(ProbabilityDistribution.normalizeLogProbabilty(Map("a" -> 2, "b" -> 3)) == Map("a" -> -1.313261687518223, "b" -> -0.31326168751822303))
  }

  test("prob") {
    assert(distr.prob("a").get === Math.exp(-1.313261687518223))
  }

  test("delegation") {
    assert(distr("b") === -0.31326168751822303)
  }
}

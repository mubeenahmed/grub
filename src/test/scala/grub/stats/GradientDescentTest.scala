package grub.stats

import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
class GradientDescentTest extends AnyFlatSpec with should.Matchers {

  val x2: Seq[Double] = Seq(1400, 1600, 1700, 1875, 1100, 1550, 2350, 2450, 1425, 1700)
  val y2: Seq[Double] = Seq(245000, 312000, 279000, 30000, 19900, 219000, 405000, 324000, 319000, 255000)

  val x = Seq(0.00, 0.22, 0.24, 0.33, 0.37, 0.44, 0.44, 0.57, 0.93, 1.00)
  val y = Seq(0.00, 0.22, 0.58, 0.20, 0.55, 0.39, 0.54, 0.53, 1.00, 0.61)

  "A double of integer x, y data" should "give the optimized weights" in {
    val weights0 = new GradientDescent(Seq(x), y).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
    val weights = List(0.1439074186030526, 0.7008910289903932)

    weights0 should not contain (Double.NaN)
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(weights0(0))
    weights(0) === weights0(0) should be (true)
    weights(1) === weights0(1) should be (true)
    for(_ <- 1 until 100) {
      val weights1 = new GradientDescent(Seq(x), y).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
      weights1 should not contain (Double.NaN)
      weights(0) === weights1(0) should be (true)
      weights(1) === weights1(1) should be (true)
    }
  }

  "A list of integer x, y data" should "give the optimized weights" in {
    val normX2 = Seq(x2.map(x => x / 100000))
    val normY2 = y2.map(y => y / 100000)

    val weights = List(2.403152355548239, 0.2740200248766769)
    val weights0 = new GradientDescent(normX2, normY2).simpleGradientDescent(iteration = 2000, learningRate = 0.005)

    weights0 should not contain (Double.NaN)
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(weights0(0))
    weights(0) === weights0(0) should be (true)
    weights(1) === weights0(1) should be (true)

    for(_ <- 1 until 100) {
      val weights1 = new GradientDescent(normX2, normY2).simpleGradientDescent(iteration = 2000, learningRate = 0.005)
      weights1 should not contain (Double.NaN)
      weights(0) === weights1(0) should be (true)
      weights(1) === weights1(1) should be (true)
    }
  }
}

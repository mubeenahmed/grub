package grub.stats

import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
class GradientDescentTest extends AnyFlatSpec with should.Matchers {

  val x2: Seq[Double] = Seq(1400, 1600, 1700, 1875, 1100, 1550, 2350, 2450, 1425, 1700)
  val y2: Seq[Double] = Seq(245000, 312000, 279000, 30800, 19900, 219000, 405000, 324000, 319000, 255000)

  val x = Seq(0.00, 0.22, 0.24, 0.33, 0.37, 0.44, 0.44, 0.57, 0.93, 1.00)
  val y = Seq(0.00, 0.22, 0.58, 0.20, 0.55, 0.39, 0.54, 0.53, 1.00, 0.61)

  "A double of integer x, y data" should "give the optimized weights" in {
    val weights0 = new GradientDescent(x, y).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(weights0._1)
    for(_ <- 1 until 100) {
      val weights1 = new GradientDescent(x, y).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
      weights0._1 === weights1._1 should be (true)
      weights0._2 === weights1._2 should be (true)
    }
  }

  "A list of integer x, y data" should "give the optimized weights" in {
    val weights0 = new GradientDescent(x2, y2).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
    for(_ <- 1 until 100) {
      val weights1 = new GradientDescent(x2, y2).simpleGradientDescent(iteration = 2000, learningRate = 0.03)
      weights0._1 === weights1._1
      weights0._2 === weights1._2
    }
  }
}

package grub.stats


import shapeless.Typeable

import scala.util.Random

/**
 * Gradient Descent for optimizing the loss and to find coefficients values for the given x and y
 * @param x
 * @param y
 * @param numeric$T$0
 * @tparam T
 */
class GradientDescent[T: Numeric](x: Seq[Seq[T]], y: Seq[T]) {

  private val numeric = implicitly[Numeric[T]]

  def stochasticGradientDescent[T](x: Seq[T], y: Seq[T], epoc: Int = 100): Double = ???

  /**
   * Simple Gradient Descent implementation, with parameter learningRate and iteration.
   * @param learningRate
   * @param iteration
   * @return
   */
  def simpleGradientDescent(learningRate: Double = 0.002,
                            iteration: Int = 1000): Seq[T] =
  {
    val i = x.size
    val m = x(0).size

    val weight0 = new Random().nextDouble().asInstanceOf[T]
    var weight1: Seq[T] = weight0 +: x.map(_ => new Random().nextDouble().asInstanceOf[T])

    val xb: Seq[Seq[T]] = x(0).map(_ => numeric.fromInt(1)) +: x
    var iter = 0
    while(iter <= iteration) {
      val weights = for {
        w <- weight1.zipWithIndex
        cost = this.hTheta(m, weight1, xb).zipWithIndex
          .map(h => numeric.minus(h._1, y(h._2)))
          .zipWithIndex
          .map(x => numeric.times(x._1.asInstanceOf[T], xb(w._2)(x._2)))
          .reduce((e1, e2) => numeric.plus(e1, e2))
        normalize = numeric.times(learningRate.asInstanceOf[T], cost).asInstanceOf[Double] / m
        newWeight = numeric.minus(w._1, normalize.asInstanceOf[T])
      } yield newWeight

      iter = iter + 1
      weight1 = weights
    }
    weight1
  }

  private def hTheta(m: Int, weights: Seq[T], xb: Seq[Seq[T]]): Seq[T] =
    for {
      mi <- 0 until m
      hT = weights.zipWithIndex
        .map(w => numeric.times(xb(w._2)(mi), w._1))
        .reduce((e1, e2) => numeric.plus(e1, e2))
    } yield hT

}

trait GD { def name: String }
case object SimpleGD extends GD {
  override def name: String = "SGD"
}
case object StochasticGD extends GD {
  override def name: String = "SGD"
}
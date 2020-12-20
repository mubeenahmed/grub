package grub.stats


import shapeless.Typeable

import scala.util.Random

class GradientDescent[T: Numeric](x: Seq[T], y: Seq[T]) {

  def stochasticGradientDescent[T](x: Seq[T], y: Seq[T], epoc: Int = 100): Double = ???

  def simpleGradientDescent(learningRate: Double = 0.002,
                            iteration: Int = 1000): (T, T) =
  {
    var weight0 = new Random().nextDouble().asInstanceOf[T]
    var weight1 = new Random().nextDouble().asInstanceOf[T]

    val numeric = implicitly[Numeric[T]]

    var iter = 0
    while(iter <= iteration) {
      val yp: Seq[T] = x.map(x0 => numeric.plus(weight0, numeric.times(x0, weight1)))

      val differences: Seq[(T, T)] = yp.zipWithIndex.map(p => {
        val diff = numeric.minus(y(p._2), p._1)
        val minus1 = numeric.fromInt(-1)
        val a = numeric.times(diff, minus1)
        val b = numeric.times(numeric.times(diff, x(p._2)), minus1)
        (a,b)
      })

      val sDa = differences.map(_._1).reduce(numeric.plus(_, _))
      val sDb = differences.map(_._2).reduce(numeric.plus(_, _))

      weight0 = numeric.minus(weight0, numeric.times(learningRate.asInstanceOf[T], sDa))
      weight1 = numeric.minus(weight1, numeric.times(learningRate.asInstanceOf[T], sDb))

      iter = iter + 1
    }

    (weight0, weight1)
  }
}

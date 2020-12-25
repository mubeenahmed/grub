package grub.stats

import grub.ds.DataFrame
import shapeless.Typeable


object Regression {

  implicit class LinearRegression[V: Typeable](val df: DataFrame[V])(implicit num: Numeric[V]) {
    def predict(values: List[Double],
                xColName: List[String],
                yColName: String,
                gdType: GD,
                learningRate: Double = 0.03,
                iteration: Int = 2000): Double = {
      val x: Seq[Seq[V]]= df.columns(xColName:_*).data
      val y: Seq[V] = df.columns(yColName).single

      val weights = gdType match {
        case SimpleGD => new GradientDescent(x, y).simpleGradientDescent(learningRate, iteration)
        case StochasticGD => ???
      }
      val numeric = implicitly[Numeric[V]]
      val calc: V = weights.drop(1)
        .zipWithIndex
        .map(x => numeric.times(values(x._2).asInstanceOf[V], x._1))
        .reduce((e1, e2) => numeric.plus(e1, e2))

      numeric.plus(calc, weights(0)).asInstanceOf[Double]
    }
  }
}

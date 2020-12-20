package grub.stats

import grub.ds.DataFrame
import shapeless.Typeable


object Regression {

  implicit class LinearRegression[V: Typeable](val df: DataFrame[V])(implicit num: Numeric[V]) {
    def predict(value: Double,
                xColName: String,
                yColName: String,
                gdType: GD,
                learningRate: Double = 0.03,
                iteration: Int = 2000): Double = {
      val x: Seq[V]= df.columns(xColName).single
      val y: Seq[V] = df.columns(yColName).single

      val (intercept, slope) = gdType match {
        case SimpleGD => new GradientDescent(x, y).simpleGradientDescent(learningRate, iteration)
      }
      val numeric = implicitly[Numeric[V]]
      numeric.plus(intercept, numeric.times(slope, value.asInstanceOf[V])).asInstanceOf[Double]
    }
  }
}

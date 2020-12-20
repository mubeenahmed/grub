package grub.stats

import grub.ds.DataFrame
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import grub.stats.Regression.LinearRegression
import org.scalactic.TolerantNumerics

class RegressionTest extends AnyFlatSpec with should.Matchers
{

  val x2: Seq[Double] = Seq(1400, 1600, 1700, 1875, 1100, 1550, 2350, 2450, 1425, 1700)
  val y2: Seq[Double] = Seq(245000, 312000, 279000, 30800, 19900, 219000, 405000, 324000, 319000, 255000)

  val x = Seq(0.00, 0.22, 0.24, 0.33, 0.37, 0.44, 0.44, 0.57, 0.93, 1.00)
  val y = Seq(0.00, 0.22, 0.58, 0.20, 0.55, 0.39, 0.54, 0.53, 1.00, 0.61)

  "Predict with linear regression" should "some future value" in {
    val df2 = DataFrame(Seq(x, y), Seq("A", "B"))

    val p = df2.predict(0.63, "A", "B", SimpleGD)
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(p)
    p should not be (Double.NaN)

    for (i <- 1 to 100) {
      val p2 = df2.predict(0.63, "A", "B", SimpleGD)
      p2 should not be (Double.NaN)
      p === p2 should be(true)
    }

  }

  it should "give prediction" in {
    val df = DataFrame(Seq(x2.map(x => x / 10000), y2.map(x => x / 10000)), Seq("House", "Price"))
    val p2 = df.predict(415000 / 10000, "House", "Price", SimpleGD)
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(p2)
    for (i <- 1 to 100) {
      val p3 = df.predict(415000 / 10000, "House", "Price", SimpleGD)
      p3 should not be (Double.NaN)
      p2 === p3 should be(true)
    }
  }
}
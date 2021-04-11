package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import grub.ds.Visualize.VisualizeImplicit
import org.scalatest.Ignore

@Ignore
class VisualizeTest extends AnyFlatSpec with should.Matchers {

  val rating = Seq(1.0, 2.0, 3.5, 2.5)
  val commenting = Seq(500.1, 55.1, 66.3, 600.1)
  val users = Seq(5.2, 2.5, 100.2, 2.1)

  "Data frame" should "should display JFrame visualize data" in {
    val df: DataFrame[Double] = DataFrame(Seq(rating, commenting, users))
    df.displayPlot(xCol = List("0", "1"), yCol = "2")
  }

}

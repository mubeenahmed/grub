package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.ds.Filtering.FilteringImplicit

class FilteringTest extends AnyFlatSpec with should.Matchers
{

  val data = Seq(Seq(1,2,3,4,5,6,7), Seq(1.036,2.202,3.98,4.11,5.6,6.3,7.2))
  val columns = Seq("A", "B")
  "Filtering" should "return matching results in function provided" in {
    val df = DataFrame(data, columns)

    val dfilter = df.columns("A") === (4)
    dfilter.data should be (Seq(Seq(4)))
  }

}

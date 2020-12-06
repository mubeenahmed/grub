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

<<<<<<< Updated upstream
=======
  "Filtering" should "return fill Na" in {
    val data = Seq(Seq("A", "", null, None), Seq(1,2,3,null))
    val df = DataFrame(data)
    val filled = df.fill(0, List("1"))

    filled.data(1) should be (Seq(1,2,3,0))

    val filled2 = df.fill(0, List("0", "1"))
    filled2.data(0) should be (Seq("A", 0, 0, 0))
    filled2.data(1) should be (Seq(1,2,3,0))


    val filled3 = df.fill("0", List("0"))
    filled3.data(0) should be (Seq("A", "0", "0", "0"))
  }
>>>>>>> Stashed changes
}

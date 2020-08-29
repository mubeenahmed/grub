package grub.structure

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.stats.BasicStatistic._

class BasicStatisticTest extends AnyFlatSpec with should.Matchers
{


  "A dataframe" should "return means values" in {
    val list = List(List(1,2,3,4), List(1.2, 4.1, 5.1, 6.3), List("A", "B", "C", "D"))
    val columns = List("Integer", "Double", "String")


    val dataFrame = DataFrame[Any](list, columns)
    val mean = dataFrame.mean[Any]

    mean(0)._2 should be (2.5)
    mean(1)._2 should be (4.175)
    mean(2)._2 should be ("NaN")
  }

  it should "return mean when sequence provided" in {
    val list = Seq(1,2,3)
    val list2 = Seq(1.5, 3, 6.1)

    val dataFrame = DataFrame[Any](List(list, list2))

    val oneList = dataFrame.data(0)
    val result = dataFrame.mean[Double](oneList)

    result should be (2.0)

    val twoList = dataFrame.data(1)
    val result2 = dataFrame.mean[Double](twoList)

    result2 should be (3.533333333333333)
  }

  it should "count the any data inside a list" in {
    val list = Seq(1,2,3)
    val list2 = Seq(1.5, 3, 6.1)

    val dataFrame = DataFrame[Any](List(list, list2))

    val count = dataFrame.count()

    count(0)._2 should be (3)
    count(1)._2 should be (3)
  }
}

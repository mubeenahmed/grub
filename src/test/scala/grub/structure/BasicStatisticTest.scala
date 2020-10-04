package grub.structure

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.stats.BasicStatistic._
import grub.stats.Information._

class BasicStatisticTest extends AnyFlatSpec with should.Matchers
{

  val single3IntElementList = Seq(1,2,3)
  val single3DoubleElementList = Seq(1.5, 3, 6.1)
  val single3StringElementList = Seq("A", "B", "C")

  val dataFrame = DataFrame[Any](List(single3IntElementList, single3DoubleElementList, single3StringElementList))


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

    val count = dataFrame.count()

    count(0)._2 should be (3)
    count(1)._2 should be (3)
  }

  it should "calculate the standard deviation and variance of a list" in {
    val std: Double = dataFrame.std[Double](single3IntElementList)
    val mean: Double = dataFrame.mean(single3IntElementList)
    val variance: Double = dataFrame.variance(single3IntElementList)

    variance should be (0.6666666666666666)
    mean should be (2)
    std should be (0.816496580927726)
  }

  it should "calculate the standard deviation and variance of a dataframe" in {
    val varancies: List[(String, Any)] = dataFrame.variance
    val stds:  List[(String, Any)] = dataFrame.std

    varancies(0)._2 should be (0.6666666666666666)
    varancies(1)._2 should be (3.668888888888888)

    stds(0)._2 should be (0.816496580927726)
    stds(1)._2 should be (1.9154343864744854)

  }

  it should "return the max values of a dataframe" in {
    val max = dataFrame.max()

    max(0)._2 should be (3)
    max(1)._2 should be (6.1)
  }

  it should "return the min values of a dataframe" in {
    val min = dataFrame.min()

    min(0)._2 should be (1)
    min(1)._2 should be (1.5)
    min(2)._2 should be ("NaN")
  }

  it should "return the description" in {
    val mean = dataFrame.mean
    val std = dataFrame.std
    val max = dataFrame.max
    val min = dataFrame.min
    val count = dataFrame.count

    val df = dataFrame.describe
    val means = df.locate("mean")
    val counts = df.locate("count")
    val maxs = df.locate("max")
    val mins = df.locate("min")

    val int = df.getWithColumns("0").singleColumn
    val dbl = df.getWithColumns("1").singleColumn
    val str = df.getWithColumns("2").singleColumn
  }
}

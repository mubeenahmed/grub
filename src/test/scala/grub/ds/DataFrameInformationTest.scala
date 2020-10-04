package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.stats.Information._

class DataFrameInformationTest  extends AnyFlatSpec with should.Matchers
{

  "A dataframe" should "return shape of object" in {
    val dataFrame: DataFrame[Any] = DataFrame(Seq(Seq(1,2,3), Seq("A","C","D")))
    val shape: (Int, Int) = dataFrame.shape
    shape should be ((3,2))
  }

  it should "return shape of object if empty" in {
    val dataFrame: DataFrame[Any] = DataFrame()
    val shape: (Int, Int) = dataFrame.shape
    shape should be ((0,0))
  }

  it should "return tail" in {
    val dataFrame:  DataFrame[Any] = DataFrame(Seq(Seq(1,2,3), Seq("A","C","D")))
    val tail = dataFrame.tail(2)
    tail.data should be (Seq(Seq(2,3), Seq("C","D")))
  }

  it should "return all all if size is greater" in {
    val dataFrame:  DataFrame[Any] = DataFrame(Seq(Seq(1,2,3), Seq("A","C","D")))
    val tail = dataFrame.tail(15)
    tail.data should be (Seq(Seq(1, 2,3), Seq("A", "C","D")))
    val head = dataFrame.head(15)
    head.data should be (Seq(Seq(1, 2,3), Seq("A", "C","D")))
  }

  it should "return head" in {
    val dataFrame:  DataFrame[Any] = DataFrame(Seq(Seq(1,2,3), Seq("A","C","D")))
    val head = dataFrame.head(2)
    head.data should be (Seq(Seq(1,2), Seq("A", "C")))
  }
}

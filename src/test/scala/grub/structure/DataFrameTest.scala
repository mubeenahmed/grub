package grub.structure

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class DataFrameTest extends AnyFlatSpec with should.Matchers {

  "A dataframe without parameter" should "return empty dataframe" in {
    val dataFrame1: DataFrame[Int] = DataFrame[Int]()
    val dataFrame2 = DataFrame()
    dataFrame1.data should be (List[Int]())
    dataFrame2.data should be (List[Nothing]())

    dataFrame1.data.size should be (0)
    dataFrame2.data.size should be (0)
  }

  it should "give a data frame without columns" in {
    val dataFrame1 = DataFrame[Int](data = List(List(1,2,3,4)))
    val dataFrame2 = DataFrame[String](data = List(List("A","B","C","D")))

    dataFrame1.data(0) should be (List(1,2,3,4))
    dataFrame1.columns.get("0") should be (0)

    dataFrame2.data(0) should be (List("A","B","C","D"))
    dataFrame2.columns.get("0") should be (0)

  }

  it should "give create list of list of rows" in {
    val list = List(1,2,3,5) //column 1
    val list2 = List("A", "B", "C", "D") // column 2
    val list3 = List(20.1, 24.1, 24.5, 26.9) // column 3

    val mixed = List(list, list2, list3)

    val dataFrame = DataFrame(mixed)

    val zero = dataFrame.columns.get("0")
    dataFrame.data(zero) should be (List(1,2,3,5))

    val one = dataFrame.columns.get("1")
    dataFrame.data(one) should be (List("A", "B", "C", "D"))

    val second = dataFrame.columns.get("2")
    dataFrame.data(second) should be (List(20.1, 24.1, 24.5, 26.9))

  }

  it should "throws exception when trying to access wrong column" in {
    val list = List(1,2,3,5) //column 1
    val dataFrame = DataFrame(List(list))

    the [IllegalArgumentException] thrownBy(dataFrame.columns.get("1")) should have message ("No valid key for column 1")
  }

  it should "name columns" in {
    val list = List(1,2,3,4)
    val dataFrame = DataFrame(data = List(list), column = Seq[String]("A"))
    dataFrame.columns.get("A") should be (0)

  }

  it should "not exceed the column size" in {
    val list = List(1,2,3,4)
    val columns = Seq[String]("A", "B", "C", "D", "Extra")

    the [IllegalArgumentException] thrownBy(DataFrame(data = List(list), column = columns)) should have message ("Column size exceeds the row size")
  }

  it should "create index when not given" in {
    val list = List(1,2,3,4)
    val list2 = List(3.1, 2.5, 21.1, 21.5)

    val columns = Seq[String]("Serial", "Value")

    val data = List(list, list2)

    val dataFrame = DataFrame(data, columns)

    dataFrame.index.index.get(0) should be (Some(0))
    dataFrame.index.index.get(1) should be (Some(1))
    dataFrame.index.index.get(2) should be (Some(2))
    dataFrame.index.index.get(3) should be (Some(3))
  }

  it should "create index when RangeIndex is give" in {
    val list = List(1,2,3,4)
    val list2 = List(3.1, 2.5, 21.1, 21.5)

    val columns = Seq[String]("Serial", "Value")

    val data = List(list, list2)

    val dataFrame = DataFrame(data, columns, RangeIndex(ends = list.size))

    dataFrame.index.index.get(0) should be (Some(0))
    dataFrame.index.index.get(1) should be (Some(1))
    dataFrame.index.index.get(2) should be (Some(2))
    dataFrame.index.index.get(3) should be (Some(3))

  }
}
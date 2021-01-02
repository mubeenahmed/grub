package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class DataFrameTest extends AnyFlatSpec with should.Matchers {

  val emptyListInt = List[Int]()
  val emptyListNothing = List[Nothing]()

  val integersList = List(List(1,2,3,4))
  val stringList = List(List("A","B","C","D"))

  val singleIntList = List(1,2,3,5)
  val singleStringList = List("A", "B", "C", "D")
  val singleDoubleList = List(20.1, 24.1, 24.5, 26.9)

  val mixed = List(singleIntList, singleStringList, singleDoubleList)

  "A dataframe without parameter" should "return empty dataframe" in {
    val dataFrame1: DataFrame[Int] = DataFrame[Int]()
    val dataFrame2 = DataFrame()
    dataFrame1.data should be (emptyListInt)
    dataFrame2.data should be (emptyListNothing)

    dataFrame1.data.size should be (0)
    dataFrame2.data.size should be (0)
  }

  it should "give a data frame without columns" in {
    val dataFrame1 = DataFrame[Int](data = integersList)
    val dataFrame2 = DataFrame[String](data = stringList)

    dataFrame1.data(0) should be (integersList(0))
    dataFrame1.columns.get("0") should be (0)

    dataFrame2.data(0) should be (stringList(0))
    dataFrame2.columns.get("0") should be (0)

  }

  it should "give create list of list of rows" in {
    val dataFrame = DataFrame(mixed)

    val zero = dataFrame.columns.get("0")
    dataFrame.data(zero) should be (singleIntList)

    val one = dataFrame.columns.get("1")
    dataFrame.data(one) should be (singleStringList)

    val second = dataFrame.columns.get("2")
    dataFrame.data(second) should be (singleDoubleList)

  }

  it should "throws exception when trying to access wrong column" in {
    val dataFrame = DataFrame(List(singleIntList))

    the [IllegalArgumentException] thrownBy(dataFrame.columns.get("1")) should have message ("No valid key for column 1")
  }

  it should "name columns" in {
    val dataFrame = DataFrame(data = List(singleIntList), column = Seq[String]("A"))
    dataFrame.columns.get("A") should be (0)

  }

  it should "not exceed the column size" in {
    val columns = Seq[String]("A", "B", "C", "D", "Extra")

    the [IllegalArgumentException] thrownBy(DataFrame(data = List(singleIntList), column = columns)) should have message ("Column size exceeds the row size")
  }

  it should "create index when not given" in {
    val columns = Seq[String]("Serial", "Value")

    val data = List(singleIntList, singleDoubleList)

    val dataFrame = DataFrame(data, columns)

    dataFrame.index.index.get(0) should be (Some(0))
    dataFrame.index.index.get(1) should be (Some(1))
    dataFrame.index.index.get(2) should be (Some(2))
    dataFrame.index.index.get(3) should be (Some(3))
  }

  it should "create index when RangeIndex is give" in {
    val columns = Seq[String]("Serial", "Value")

    val data = List(singleIntList, singleDoubleList)

    val dataFrame = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))

    dataFrame.index.index.get(0) should be (Some(0))
    dataFrame.index.index.get(1) should be (Some(1))
    dataFrame.index.index.get(2) should be (Some(2))
    dataFrame.index.index.get(3) should be (Some(3))

  }

  it should "locate list by index" in {
    val columns = Seq[String]("Serial", "Value")
    val data = List(singleIntList, singleDoubleList)
    val dataFrame = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))

    val byOneIndex = dataFrame.locate(0)
    byOneIndex.data(0)(0) should be (1)
    byOneIndex.data(1)(0) should be (20.1)

    val byTwoIndex = dataFrame.locate(1)
    byTwoIndex.data(0)(0) should be (2)
    byTwoIndex.data(1)(0) should be (24.1)
  }

  it should "locate list by seq index" in {
    val columns = Seq[String]("Serial", "Value")
    val data = List(singleIntList, singleDoubleList)
    val dataFrame = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))

    val row = dataFrame.locateRow(0)
    row.data should be (Seq(Seq(1), Seq(20.1)))
  }

  it should "return column(s)" in {
    val columns = Seq[String]("Serial", "Value")
    val data = List(singleIntList, singleDoubleList)
    val dataFrame = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))

    val byColumn = dataFrame.columns("Serial", "Value")
    byColumn.data(0)(0) should be (1)
    byColumn.data(0)(1) should be (2)
    byColumn.data(0)(2) should be (3)

    byColumn.data(1)(0) should be (20.1)
    byColumn.data(1)(1) should be (24.1)
    byColumn.data(1)(2) should be (24.5)
  }

  it should "convert the type of dataframe" in {
    val columns = Seq[String]("Serial", "Value")
    val data = List(singleIntList, singleDoubleList)
    val dataFrame: DataFrame[Any] = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))

    dataFrame.asType[Int]("Serial").data should be (Seq(Seq(1,2,3,5)))
    dataFrame.asType[Double]("Value").data should be (Seq(Seq(20.1, 24.1, 24.5, 26.9)))

    the [IllegalArgumentException] thrownBy(dataFrame.asType[Double]("Random X")) should have message ("No valid key for column Random X")
  }

  it should "print the dataframe" in {
    val columns = Seq[String]("Serial", "Value", "Value 2", "Value 3")
    val data = List(singleIntList, singleDoubleList, Seq("Long string 1", "Long string 2", "Long string 3", "Long string 4"),
      Seq(1.00, 2.00, 3.00, 4.00))
    val dataFrame: DataFrame[Any] = DataFrame(data, columns, RangeIndex(ends = singleIntList.size))
    dataFrame.print
  }

}
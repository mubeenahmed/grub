package grub.ds

import java.time.LocalDateTime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import grub.ds.CrudOps.CrudOpsImplicit

class CrudOpsTest extends AnyFlatSpec with should.Matchers {

  val columns = List("A", "B", "C")
  val df = DataFrame(Seq(Seq(1,4,3), Seq("abc", "csd", "xtz"), Seq(1.2, 3.6, 1.7)), columns)

  "Adding column" should "add new columns with data" in {
    val df2 = df.addCol("D", Seq[Double](5.1, 5.2, 6.1))
    val column = df2.columns.get("D")

    val data = df2.columns("D").single
    column should be (3)
    data should be (Seq(5.1, 5.2, 6.1))
  }

  val newRow = Seq('c', 'd', 'e')
  "Adding row" should "add new row with data" in {
    val df3 = df.addRow(newRow)
    val frame = df3.locate(4)
    frame.data should be (Seq(Seq('c'), Seq('d'), Seq('e')))
    frame.index.get(4) should be (3)
  }

  it should "add new index on array based index" in {
    val dataFrame = DataFrame(df.data, columns, ArrayBasedIndex(Array("x", "y", "z")))
    val df3 = dataFrame.addRow(newRow)

    val dataFrame2 = DataFrame(df.data, columns, ArrayBasedIndex(Array(1,4,3)))
    val df4 = dataFrame2.addRow(newRow)

    df3.index.get(4) should be (3)
    df4.index.get(4) should be (3)
  }

  it should "add new index on date based index" in {
    val dataFrame = DataFrame(df.data, columns, DateTimeIndex(LocalDateTime.now(), 3))
    val df3 = dataFrame.addRow(newRow)

    df3.index.index(4) should be (3)
  }

  "Removing column" should "delete the new column" in {
    val colName = "C"
    val dfWithDeletedRecord = df.deleteColumn(colName)
    the [IllegalArgumentException] thrownBy(dfWithDeletedRecord.columns.get(colName))
  }

  "Removing row" should "delete the row" in {
    val index = 1
    val removedRow = df.deleteRow(index)
    the [IllegalArgumentException] thrownBy(removedRow.locate(index))
  }

  "Appending dataframe" should "append rows with their index" in {
    val appended = df.append(df)
    appended.data(0) should be (Seq(1,4,3,1,4,3))
  }

  "Adding uneven dataframes by columns" should "throw exception" in {
    val unevenDataFrame = DataFrame[Any](Seq(Seq(1,2,4)), List("A"))
    the [IllegalArgumentException] thrownBy(df.append(unevenDataFrame))
  }

  "Adding similar indexes name" should "append with same index" in {
    //TODO
  }
}
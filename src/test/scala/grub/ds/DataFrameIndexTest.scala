package grub.ds

import java.time.LocalDateTime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class DataFrameIndexTest extends AnyFlatSpec with should.Matchers  {

  "array-based index" should "give mapping" in {
    val arr = Array(1,4,3,5,8,9)
    val arrayBasedIndex = ArrayBasedIndex(arr)

    arrayBasedIndex.index.get(4).get should be (1)
    arrayBasedIndex.index.get(1).get should be (0)
    arrayBasedIndex.index.get(3).get should be (2)
    arrayBasedIndex.index.get(5).get should be (3)
    arrayBasedIndex.index.get(8).get should be (4)
  }

  it should "create empty array-based index" in {
    val empty = Array()
    val arrayBasedIndex = ArrayBasedIndex[Nothing](empty)
    arrayBasedIndex.index.isEmpty should be (true)
  }

  "range-based index" should "give mapping" in {
    val range = RangeIndex(5, 10)
    range.index.get(5).get should be (0)
    range.index.get(6).get should be (1)
    range.index.get(7).get should be (2)
    range.index.get(8).get should be (3)
    range.index.get(9).get should be (4)
  }

  it should "give empty range index" in {
    val range = RangeIndex(ends = 0)
    range.index.isEmpty should be (true)
  }

  val localDateTime = LocalDateTime.of(1992, 7, 15, 0, 0, 0)
  "date-base index" should "give date index" in {
    val dateRange = DateTimeIndex(localDateTime, 10)

    val nextDay = localDateTime.plusDays(1)
    val next9Day = localDateTime.plusDays(9)
    dateRange.index.get(localDateTime).get should be (0)
    dateRange.index.get(nextDay).get should be (1)
    dateRange.index.get(next9Day).get should be (9)
  }

  it should "give empty index" in {
    val dataFrame = DateTimeIndex(localDateTime, 0)
    dataFrame.index.isEmpty should be (true)
  }

  "Empty index" should "give" in {
    val empty = EmptyIndex
    empty.index.isEmpty should be (true)
  }
}

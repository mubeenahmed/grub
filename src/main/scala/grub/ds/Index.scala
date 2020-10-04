package grub.ds

import java.time.LocalDateTime

import scala.collection.mutable

/**
 * Index with unimplemented index. To create your own index extends this trait. The index is the LinkedHasHap[Any, Int]
 * similar to [[grub.ds.Columns]]
 */
trait Index {
  val index: mutable.LinkedHashMap[Any, Int]
  def get[T](t: T): Int = index.getOrElse(t, throw new IllegalArgumentException(s"No index found ${t}"))
}

/**
 * RangeIndex is index with start and end of the index.
 * @param start
 * @param ends
 */
case class RangeIndex(start: Int = 0, ends: Int) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indices: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    LazyList.range(start, ends).zipWithIndex.foreach(i => indices.put(i._1, i._2))
    indices
  }
}

/**
 * The datatime index gets the LocalDateTime with periods, it will return the index in datetime
 * @param start
 * @param period
 */
case class DateTimeIndex(start: LocalDateTime, period: Int) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indicies: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    LazyList.range(0, period).foreach(i => indicies.put(start.plusDays(i), i))
    indicies
  }
}

/**
 * The index will return index by the array provided.
 * @param arr
 * @tparam T
 */
case class ArrayBasedIndex[T](arr: Array[T]) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indicies: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    arr.zipWithIndex.foreach(x => indicies.put(x._1, x._2))
    indicies
  }
}

/**
 * The index with empty LinkedHashMap[Any, Int]
 */
case object EmptyIndex extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap[Any, Int]()
}

package grub.structure

import java.time.LocalDateTime

import scala.collection.mutable

trait Index {
  val index: mutable.LinkedHashMap[Any, Int]
}

case class RangeIndex(start: Int = 0, ends: Int) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indices: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    LazyList.range(start, ends).foreach(i => indices.put(i, i))
    indices
  }
}

case class DateTimeIndex(start: LocalDateTime, period: Int) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indicies: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    LazyList.range(0, period).foreach(i => indicies.put(start.plusDays(i), i))
    indicies
  }
}

case class ArrayBasedIndex[T](arr: Array[T]) extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = {
    val indicies: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    for((x, i) <- arr.zipWithIndex) indicies(x, i)
    indicies
  }
}

case object EmptyIndex extends Index {
  override val index: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap[Any, Int]()
}

//object Index {
//  def apply(x: RangeIndex): L = x.index
//}


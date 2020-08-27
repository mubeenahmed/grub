package grub.structure

import scala.collection.mutable

class Columns(data: mutable.LinkedHashMap[Any, Int])
{
  def get(value: Any): Int =
    data.getOrElse(value, throw new IllegalArgumentException(s"No valid key for column ${value}"))

  def all: List[String] = data.keysIterator.map(x => x.toString).toList
}

object Columns {

  def apply(): Columns = new Columns(new mutable.LinkedHashMap())

  def apply(size: Int): Columns = {
    val columns: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    LazyList.range(0, size).foreach(i => columns.put(i, i))
    new Columns(columns)
  }

  def apply(cols: Seq[Any], rowSize: Int): Columns = {
    if(cols.size > rowSize) {
      throw new IllegalArgumentException("Column size exceeds the row size")
    }
    val columns: mutable.LinkedHashMap[Any, Int] = new mutable.LinkedHashMap()
    for((x, i) <- cols.zipWithIndex) columns.put(x, i)
    new Columns(columns)
  }

}

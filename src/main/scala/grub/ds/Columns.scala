package grub.ds

import scala.collection.mutable

/**
 * Columns are to get the list of all rows in the column. It takes mutable hashmap to hold name and index, index is the
 * location of the data's index.
 * @constructor creates a mutable.LinkedHashMap[String, Int]
 * @param data
 */
class Columns(data: mutable.LinkedHashMap[String, Int])
{
  /**
   * Get the index of the column
   * @param value
   * @return
   */
  def get(value: String): Int =
    data.getOrElse(value, throw new IllegalArgumentException(s"No valid key for column ${value}"))

  /**
   * Fetch all the columns in the dataframe
   * @return
   */
  def all: List[String] = data.keysIterator.map(x => x.toString).toList
}

/** Factory for [[grub.ds.Columns]] instances. */
object Columns {

  /**
   * Create the empty columns
   * @return
   */
  def apply(): Columns = new Columns(new mutable.LinkedHashMap())

  /**
   * Create the column with size of the int. It iterates till the size and add integer to hashmap
   * @param size
   * @return
   */
  def apply(size: Int): Columns = {
    val columns: mutable.LinkedHashMap[String, Int] = new mutable.LinkedHashMap()
    LazyList.range(0, size).foreach(i => columns.put(i.toString, i))
    new Columns(columns)
  }

  /**
   * Create column with the name provided, the rowSize parameter is for validating whether column size provided
   * is not greater then actualColSize
   * @param cols
   * @param actualColSize
   * @return
   */
  def apply(cols: Seq[String], actualColSize: Int): Columns = {
    if(cols.size > actualColSize) {
      throw new IllegalArgumentException("Column size exceeds the row size")
    }
    val columns: mutable.LinkedHashMap[String, Int] = new mutable.LinkedHashMap()
    for((x, i) <- cols.zipWithIndex) columns.put(x, i)
    new Columns(columns)
  }

}

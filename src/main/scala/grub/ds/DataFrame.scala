package grub.ds

/**
 * A concrete class for manipulating data in rows and column style. This class holds the data and behavior
 * to manipulate the data. The generic type here can be either Int or Double or String
 * (if all the data is integer or double or string) otherwise use Any
 * @constructor data which is 2D sequence, columns the header of data, index the rows pointers
 * @param data is the Seq[Seq[V]], more than Seq in Seq is the column for example Seq(Seq(1,2,4), Seq(5,1,5)), it is two columns and three rows
 * @param columns the name of the columns for accessing columns
 * @param index the index for accessing rows
 * @tparam V generic type for data
 */
class DataFrame[V](val data: Seq[Seq[V]],
                   val columns: Columns,
                   val index: Index)
{

  /**
   * Locate by index, pass the name of the index, it will return the dataframe with that rows in that index
   * @param name
   * @tparam T
   * @return
   */
  def locate[T](name: T): DataFrame[V] = {
    val foundIndex = index.get(name)
    val list: Seq[Seq[V]] = data.map(x => Seq(x(foundIndex)))
    DataFrame(list, columns.all, index)
  }

  /**
   * Locate by index of seq of data. It can be used if row index is known
   * @param seqIndex
   * @tparam T
   * @return
   */
  def locateRow[T](seqIndex: Int): DataFrame[V] = {
    val list: Seq[Seq[V]] = data.map(x => Seq(x(seqIndex)))
    DataFrame(list, columns.all, index)
  }

  /**
   * Get the columns, pass one or list of string, it will return dataframe with columns passed
   * @param names
   * @return
   */
  def columns(names: String*): DataFrame[V] = {
    val columnIndex: Seq[Int] = names.map(x => columns.get(x))
    val selectedColumns = columnIndex.map(x => data(x))
    DataFrame(selectedColumns, names, index)
  }

  /**
   * To get single column in the dataframe
   * @return
   */
  def single: Seq[V] = data(0)
}

/**
 * Factory for [[grub.ds.DataFrame]] instances.
 */
object DataFrame
{
  /**
   * Creates the empty dataframe
   * @tparam V
   * @return
   */
  def apply[V](): DataFrame[V] = new DataFrame[V](Seq(), Columns(), EmptyIndex)

  /**
   * Creates datdframe with data and default columns and default index
   * @param data
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]]) = new DataFrame[V](data, Columns(data.size), RangeIndex(ends = data(0).size))

  /**
   * Creates dataframe with data, specified columns and default index
   * @param data
   * @param column
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]], column: Seq[String]): DataFrame[V] = new DataFrame[V](data, Columns(column, data.size), RangeIndex(ends = data(0).size))

  /**
   * Creates dataframe with data, specified columns and specified index
   * @param data
   * @param columns
   * @param index
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]], columns: Seq[String], index: Index): DataFrame[V] = new DataFrame[V](data, Columns(columns, data.size), index)
}
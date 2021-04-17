package grub.ds

/**
 * A concrete class for manipulating data in rows and columns style. This class holds the data and behavior
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

  /**
   * When multiple different type of data is used, the type used are parent class, for example
   * if your dataframe contains mixture of integers and doubles, floats then the dataframe type will be AnyVal
   * if your dataframe contains string and mixture of integer and doubles then the dataframe type will be Any
   * therefore to used a single column with explicily defining their type can be handy.
   */
  def asType[T](column: String): DataFrame[T] =
    DataFrame(Seq(this.columns(column).data(0).map(_.asInstanceOf[T])), List(column))

  def print: Unit = {
    for(_ <- 0 to columns.all.reduce((e1, e2) => e1 + e2).size) {
      printf("==")
    }
    println()
    printf("Indexes | ")
    for(name <- columns.all) {
      printf(s"""| ${name} """)
    }
    printf(" | ")
    println()

    val i = index.index.map(x => x._1)
    val d = Seq(i) ++ data
    for(row <- d(0).zipWithIndex) {
      for(col <- data) {
        printf(" | ")
        printf(col(row._2).toString)
        printf(" | ")
      }
      println()
    }
  }
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
  def apply[V](): DataFrame[V] =
    new DataFrame[V](Seq(), Columns(), EmptyIndex)

  /**
   * Creates datdframe with data and default columns and default index
   * @param data
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]]) =
    new DataFrame[V](data, Columns(data.size), RangeIndex(ends = data(0).size))

  /**
   * Creates dataframe with data, specified columns and default index
   * @param data
   * @param column
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]], column: Seq[String]): DataFrame[V] =
    new DataFrame[V](data, Columns(column, data.size), RangeIndex(ends = data(0).size))

  /**
   * Creates dataframe with data, specified columns and specified index
   * @param data
   * @param columns
   * @param index
   * @tparam V
   * @return
   */
  def apply[V](data: Seq[Seq[V]], columns: Seq[String], index: Index): DataFrame[V] =
    new DataFrame[V](data, Columns(columns, data.size), index)
}
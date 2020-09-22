package grub.structure

class DataFrame[V](val data: Seq[Seq[V]],
                   val columns: Columns,
                   val index: Index)
{

  def locate[T](name: T): DataFrame[V] = {
    val foundIndex = index.index.getOrElse(name, throw new IllegalArgumentException(s"No valid key for index ${name}"))
    val list: Seq[Seq[V]] = data.map(x => Seq(x(foundIndex)))
    DataFrame(list, columns.all, index)
  }

  def getWithColumns(names: String*): DataFrame[V] = {
    //TODO write unit test
    val columnIndex: Seq[Int] = names.map(x => columns.get(x))
    val selectedColumns = columnIndex.map(x => data(x))
    DataFrame(selectedColumns, names, index)
  }

  def singleColumn: Seq[V] = data(0)
}

object DataFrame
{
  def apply[V](): DataFrame[V] = new DataFrame[V](Seq(), Columns(), EmptyIndex)

  def apply[V](data: Seq[Seq[V]]) = new DataFrame[V](data, Columns(data.size), RangeIndex(ends = data(0).size))

  def apply[V](data: Seq[Seq[V]], column: Seq[String]): DataFrame[V] = new DataFrame[V](data, Columns(column, data.size), RangeIndex(ends = data(0).size))

  def apply[V](data: Seq[Seq[V]], columns: Seq[String], index: Index): DataFrame[V] = new DataFrame[V](data, Columns(columns, data.size), index)
}
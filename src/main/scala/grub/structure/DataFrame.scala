package grub.structure

import scala.collection.mutable

class DataFrame[V](val data: Seq[Seq[V]],
                   val columns: Columns,
                   val index: Index)
{

}

object DataFrame
{
  def apply[V](): DataFrame[V] = new DataFrame[V](Seq(), Columns(), EmptyIndex)

  def apply[V](data: Seq[Seq[V]]) = new DataFrame[V](data, Columns(data.size), RangeIndex(ends = data.size))

  def apply[V](data: Seq[Seq[V]], column: Seq[String]): DataFrame[V] = new DataFrame[V](data, Columns(column, data.size), RangeIndex(ends = data.size))

  def apply[V](data: Seq[Seq[V]], columns: Seq[String], index: Index): DataFrame[V] = new DataFrame[V](data, Columns(columns, data.size), index)
}
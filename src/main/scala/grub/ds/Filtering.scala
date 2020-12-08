package grub.ds

import grub.ds.CrudOps.CrudOpsImplicit
import shapeless.Typeable

/**
 * Filtering the rows of the dataframe. This object provides method to filter out with different condition. Please note,
 * the filtering can be implemented easily if needed
 */
object Filtering {

  /**
   * Implicit helpers for [[grub.ds.DataFrame]] instances. This provide the actual implementation for filtering
   * functions
   * {{{
   *   import BasicStatistic._
   *   val means = df.mean()
   * }}}
   *
   * @param dataFrame
   * @tparam V
   */
  implicit class FilteringImplicit[V: Typeable](val dataFrame: DataFrame[V]) {

    def fill[T <: V](value: T, columns: List[String]): DataFrame[V] = {
      val indexes: List[Int] =
        columns.map(x => dataFrame.columns.get(x))

      val data: Seq[Seq[V]] = dataFrame.data.zipWithIndex.collect {
        case (x, i) if indexes.indexOf(i) >= 0 => x.map(e => if(isNAN(e)) value else e)
        case (o, _) => o
      }
      DataFrame(data, dataFrame.columns.all)
    }

    def drop[T <: V](value: T): DataFrame[V] = {
      val newData = dropByIndex(value, dataFrame.data)
      DataFrame(newData)
    }

    def filter[T <: Double](fn: V => Boolean): DataFrame[V] = {
      val df: Seq[Seq[V]] = dataFrame.data.map(x => x.filter(y => fn(y)))
      DataFrame(df, dataFrame.columns.all)
    }

    private def isNAN(e: V): Boolean = e == null || (e != null && e == "") || (e != null && e == None)

    private def dropByIndex[T](value: T, data: Seq[Seq[V]]): Seq[Seq[V]] = {

      val indexes: Seq[Int] = for {
        first <- data
        index <- first.zipWithIndex if index._1 == value
      } yield index._2

      if(indexes.size == 0) {
        data
      }
      else {
        val index = indexes.take(1)(0)
        val newDF = data.map(x => x.zipWithIndex.filter(e => e._2 != index).map(x => x._1))
        dropByIndex(value, newDF)
      }
    }
  }
}

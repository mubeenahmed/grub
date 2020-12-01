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

    def filter[T <: Double](fn: V => Boolean): DataFrame[V] = {
      val df: Seq[Seq[V]] = dataFrame.data.map(x => x.filter(y => fn(y)))
      DataFrame(df, dataFrame.columns.all)
    }

  }
}

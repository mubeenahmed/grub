package grub.stats

import grub.structure.{ArrayBasedIndex, DataFrame}
import shapeless.Typeable
import BasicStatistic.BasicStatisticImplicits

/**
 * Gets the methods for finding insight of dataframe
 */
object Information {

  /**
   * Implicit helpers for [[grub.structure.DataFrame]] instances. This provide the actual implementation for
   * dataframe information
   * {{{
   *   import BasicStatistic._
   *   val means = df.mean()
   * }}}
   * @param dataFrame
   * @tparam V
   */
  implicit class InformationImplicits[V: Typeable](val dataFrame: DataFrame[V])
  {
    def shape: (Int, Int) = dataFrame.data.size match {
      case 0 => (0, 0)
      case _ => (dataFrame.data(0).size, dataFrame.columns.all.size)
    }

    def tail(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.takeRight(n)), dataFrame.columns.all)
    def head(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.take(n)), dataFrame.columns.all)


    def describe: DataFrame[Any] = {
      val columns = dataFrame.columns
      val arr = Array("mean", "std", "max", "min", "count") //, "25%", "50%", "75%", "max")
      val index = ArrayBasedIndex(arr)

      val mean = dataFrame.mean[V].map(x => x._2)
      val std = dataFrame.std[V].map(x => x._2)
      val max = dataFrame.max.map(x => x._2)
      val min = dataFrame.min.map(x => x._2)
      val count = dataFrame.count.map(x => x._2)

      val list= columns.all
        .zipWithIndex
        .map(x => Seq(mean(x._2), std(x._2), max(x._2), min(x._2), count(x._2)))
      val df = DataFrame(list, columns.all, index)
      df
    }

  }
}

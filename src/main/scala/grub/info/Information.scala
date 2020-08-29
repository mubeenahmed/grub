package grub.info

import grub.structure.{ArrayBasedIndex, DataFrame}
import grub.stats.BasicStatistic._

object Information {

  implicit class Information[V](val dataFrame: DataFrame[V])
  {
    def shape: (Int, Int) = dataFrame.data.size match {
      case 0 => (0, 0)
      case _ => (dataFrame.data(0).size, dataFrame.columns.all.size)
    }

    def tail(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.takeRight(n)), dataFrame.columns.all)
    def head(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.take(n)), dataFrame.columns.all)


    def describe: Unit = {
    //DataFrame[V] = {
      val columns = dataFrame.columns
      val arr = Array("count", "mean", "std", "min", "25%", "50%", "75%", "max")
      val index = ArrayBasedIndex(arr)
//      val data: List[(Any, Any)] = dataFrame.mean()


    }

  }
}
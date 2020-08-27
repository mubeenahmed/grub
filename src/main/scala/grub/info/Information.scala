package grub.info

import grub.structure.DataFrame
import org.breeze.stats.structure.DataFrame


object Information {

  implicit class Information[V](val dataFrame: DataFrame[V])
  {
    def shape: (Int, Int) = dataFrame.data.size match {
      case 0 => (0, 0)
      case _ => (dataFrame.data(0).size, dataFrame.columns.all.size)
    }

    def tail(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.takeRight(n)), dataFrame.columns.all)
    def head(n: Int = 5): DataFrame[V] = DataFrame(dataFrame.data.map(x => x.take(n)), dataFrame.columns.all)


//    def describe: DataFrame[V] = {
//      dataFrame.data
//    }


  }

}
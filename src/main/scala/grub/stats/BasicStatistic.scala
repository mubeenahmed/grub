package grub.stats

import com.sun.tools.javac.code.TypeTag
import grub.structure.DataFrame

import shapeless._

object BasicStatistic {
  implicit class BasicStatistic[V: Typeable](dataFrame: DataFrame[V]) {
    def mean[T](): List[(String, T)] = {
      dataFrame
        .columns
        .all
        .map(x => (x, mean(dataFrame.getWithColumns(x).singleColumn)))
    }

    def mean[T](x: Seq[V]): T = {
      val intType = TypeCase[Seq[Int]]
      val doubleType = TypeCase[Seq[Double]]
      val stringType = TypeCase[Seq[String]]
      val charType = TypeCase[Seq[Char]]

      val result: T = (x match {
        case intType(l) => l.reduce(_ + _).toDouble / l.size
        case doubleType(l) => l.reduce[Double](_ + _) / l.size
        case stringType(_) => "NaN"
        case charType(_) => "NaN"
        case _ => throw new IllegalArgumentException("No type supported")
      }).asInstanceOf[T]
      result
    }


    def count(): List[(Any, Int)] = {
      dataFrame.columns.all
        .map(x => (x, dataFrame.data(dataFrame.columns.get(x)).size))
    }
  }

}

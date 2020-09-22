package grub.stats

import com.sun.tools.javac.code.TypeTag
import grub.structure.DataFrame

import shapeless._

object BasicStatistic {

  private val listOfIntType = TypeCase[Seq[Int]]
  private val listOfDoubleType = TypeCase[Seq[Double]]
  private val listOfStringType = TypeCase[Seq[String]]
  private val listOfCharType = TypeCase[Seq[Char]]
  private val doubleType = TypeCase[Double]
  private val stringType = TypeCase[String]

  implicit class BasicStatistic[V: Typeable](dataFrame: DataFrame[V]) {
    def mean[T]: List[(String, T)] = dataFrame
      .columns
      .all
      .map(x => (x, mean(dataFrame.getWithColumns(x).singleColumn)))

    def std[T](): List[(String, T)] = dataFrame
      .columns
      .all
      .map(x => (x, std(dataFrame.getWithColumns(x).singleColumn)))

    def variance[T]: List[(String, T)] = dataFrame
      .columns
      .all
      .map(x => (x, variance(dataFrame.getWithColumns(x).singleColumn)))

    def mean[T](x: Seq[V]): T = (x match {
        case listOfIntType(l) => l.reduce(_ + _).toDouble / l.size
        case listOfDoubleType(l) => l.reduce[Double](_ + _) / l.size
        case listOfStringType(_) => "NaN"
        case listOfCharType(_) => "NaN"
        case _ => throw new IllegalArgumentException("No type supported")
      }).asInstanceOf[T]

    def std[T](x: Seq[V], ddof: Int = 0): T = {
      val v: T = variance(x, ddof)
      (v match {
        case stringType(_) => "NaN"
        case doubleType(x) => Math.sqrt(x)
      }).asInstanceOf[T]
    }

    def variance[T](value: Seq[V], ddof: Int = 0) = {
      val calculatedMean: Any = mean(value)
      (value match  {
        case listOfIntType(l) => l.foldRight[Double](0.0)((x, y) => Math.pow(x - calculatedMean.asInstanceOf[Double], 2) + y) / l.size - ddof
        case listOfDoubleType(l) => l.foldRight[Double](0.0)((x, y) => Math.pow(x - calculatedMean.asInstanceOf[Double], 2) + y) / l.size - ddof
        case listOfStringType(_) => "NaN"
        case listOfCharType(_) => "NaN"
      }).asInstanceOf[T]
    }

    def count(): List[(String, Int)] = {
      dataFrame.columns.all
        .map(x => (x, dataFrame.data(dataFrame.columns.get(x)).size))
    }

    def min(): List[(String, V)] =
      dataFrame.columns.all
      .map(x => (x, min(dataFrame.getWithColumns(x).singleColumn)))

    def max(): List[(String, V)] =
      dataFrame.columns.all
      .map(x => (x, max(dataFrame.getWithColumns(x).singleColumn)))

    def min(value: Seq[V]): V = value match {
      case listOfIntType(l) => l.min.asInstanceOf[V]
      case listOfDoubleType(d) => d.min.asInstanceOf[V]
      case _ => "NaN".asInstanceOf[V]
    }

    def max(value: Seq[V]): V = value match {
      case listOfIntType(l) => l.max.asInstanceOf[V]
      case listOfDoubleType(d) => d.max.asInstanceOf[V]
      case _ => "NaN".asInstanceOf[V]
    }

  }

}

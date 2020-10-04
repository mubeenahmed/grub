package grub.io

import java.util.regex.Pattern

import grub.ds.{ArrayBasedIndex, DataFrame, Index}

import scala.io.Source

/**
 * CSVReadOption is the data ds which is holding information about the how to read CSV files
 * @param fileName The name and the path of the file
 * @param delimiter The delimiter for the csv file
 * @param includeHeader Should include the heading of the csv, or you are providing your own
 * @param names The names of the columns, if you don't want csv to read columns by own
 * @param indexCol The name of index, if you want to make a column act as index
 */
case class CSVReadOption(fileName: String,
                            delimiter: String = ";",
                            includeHeader: Boolean = true,
                            names: Option[List[String]] = None,
                            indexCol: Option[String] = None)

/**
 * This will read the csv file and convert them into the data frame
 */
class CSVRead {

  this: GrubIO =>

  /**
   * Reads the csv file
   * @param option
   * @return
   */
  def read(option: CSVReadOption): DataFrame[Any] = {
    val src = source(option.fileName)
    val columnNames = includeHeader(src, option)
    val columns: List[(String, Int)] = columnNames.zipWithIndex

    val body: Seq[Array[String]] = src.map(_.split(option.delimiter)).toSeq

    val colIndex  = getIndex(columnNames, option)
    val index = addIndex(colIndex, body)

    val data = getSeq(columns, body)

    if(index.isDefined) DataFrame[Any](data, columnNames, index.get)
    else DataFrame[Any](data, columnNames)
  }

  private def getSeq(columns: List[(String, Int)], body: Seq[Array[String]]): Seq[Seq[Any]]  = for {
    (_, c) <- columns
    cols = body.map {
      case x: Array[String] =>
        if(isIntType(x(c))) getIntType(x(c))
        else if (isDoubleType(x(c))) getDoubleType(x(c))
        else x(c)
    }
  } yield cols

  private def getIndex(columnNames: List[String], option: CSVReadOption): Int =
    option.indexCol match {
      case Some(x) => columnNames.indexOf(x)
      case None => -1
    }

  private def addIndex(colIndex: Int, body: Seq[Array[String]]): Option[ArrayBasedIndex[String]] =
    if(colIndex == -1) None
    else  Some(ArrayBasedIndex(body.map(x => x(colIndex)).toArray))

  private def includeHeader(src: Iterator[String], option: CSVReadOption): List[String] =
    if(option.includeHeader) src.take(1).map(_.split(option.delimiter)).next().toList
    else option.names.getOrElse(throw new IllegalArgumentException("Add columns names"))


  private def getIntType(value: String): Int = value.toInt
  private def getDoubleType(value: String): Double = value.toDouble

  private def isIntType(value: String): Boolean = try {
    value.toInt
    true
  } catch {
    case _: Exception => false
  }

  private def isDoubleType(value: String): Boolean = try {
    value.toDouble
    true
  } catch {
    case _: Exception => false
  }

}

/**
 * GrubIO can be used if you want to implement your on source
 */
trait GrubIO {
  def source(fileName: String): Iterator[String]
}

/**
 * FileSystem to extract and read the file
 */
trait FileSystem extends GrubIO {
  override def source(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines
}

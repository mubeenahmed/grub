package grub.io

import grub.structure.{ArrayBasedIndex, DataFrame, Index}

import scala.io.Source


case class CSVReadOption(fileName: String,
                            delimiter: String = ";",
                            includeHeader: Boolean = true,
                            names: Option[List[String]] = None,
                            indexCol: Option[String] = None)
class CSVRead {
  this: GrubIO =>
  def read(option: CSVReadOption): DataFrame[String] = {
    val src = source(option.fileName)
    val columnNames = includeHeader(src, option)
    val columns: List[(String, Int)] = columnNames.zipWithIndex

    val body: Seq[Array[String]] = src.map(_.split(option.delimiter)).toSeq

    val data = getSeq(columns, body)
    val colIndex  = getIndex(columnNames, option)
    val index = addIndex(colIndex, body)

    if(index.isDefined) DataFrame[String](data, columnNames, index.get)
    else DataFrame[String](data, columnNames)
  }

  private def getSeq(columns: List[(String, Int)], body: Seq[Array[String]]): Seq[Seq[String]]  = for {
    (_, c) <- columns
    cols = body.map(x => x(c))
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
}

trait GrubIO {
  def source(fileName: String): Iterator[String]
}

trait FileSystem extends GrubIO {
  override def source(fileName: String): Iterator[String] = Source.fromFile(fileName).getLines
}

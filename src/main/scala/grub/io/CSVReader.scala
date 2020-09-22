package grub.io

import grub.structure.{DataFrame, Index}

object CSVReader {

  def read(fileName: String): DataFrame[Any] = {
    val df = DataFrame(Seq(Seq[Any](1,2,4)))
    df
  }

//  def read(fileName: String,
//           sep: String = ",",
//           delimiter: String = ";"): DataFrame[Any] = ???
//
//  def read(fileName: String,
//           sep: String = ",",
//           delimiter: String = ";",
//           includeHeader: Boolean = true,
//           names: Option[List[String]] = None,
//           indexCol: Option[Index] = None): DataFrame[Any] = ???
}

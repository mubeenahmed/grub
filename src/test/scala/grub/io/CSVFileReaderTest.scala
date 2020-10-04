package grub.io

import grub.io
import grub.ds.DataFrame
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import grub.stats.Information.InformationImplicits

class CSVFileReaderTest extends AnyFlatSpec with should.Matchers
{

  val csvReader = new CSVRead with MockFileSystem
  val csvReader2 = new CSVRead with MockFileSystem2

  val fileName = "/c:/sample.csv"

  "Reading from default option" should "create dataframe" in {
    val csvReadingOption = CSVReadOption(fileName)
    val df: DataFrame[Any] = csvReader.read(csvReadingOption)
    df.data should be (Seq(Seq(1), Seq(4), Seq(0.1)) )
  }


  "Reading with specifying column" should "create dataframe" in {
    val columnNames = List("a", "b", "c")
    val csvReadOption = io.CSVReadOption(fileName = fileName, includeHeader = false, names = Some(columnNames))
    val df: DataFrame[Any] = csvReader.read(csvReadOption)
    df.columns.all should be (columnNames)
    df.data should be (Seq(Seq("cat", 1), Seq("dog", 4), Seq("mouse", 0.1)) )
  }

  it should "throw IllegalArgumentException " in {
    val csvReaderOption = io.CSVReadOption(fileName = fileName, includeHeader = false)
    the [IllegalArgumentException] thrownBy(csvReader.read(csvReaderOption))
  }

  it should "use csv header" in {
    val columnNames = List("a", "b", "c")
    val csvReaderOption = io.CSVReadOption(fileName = fileName, names = Some(columnNames))
    val df: DataFrame[Any] = csvReader.read(csvReaderOption)
    df.data should be (Seq(Seq(1), Seq(4), Seq(0.1)) )
  }


  "Reading by specifying index" should "create index" in {
    val csvReaderOption1 = io.CSVReadOption(fileName = fileName, indexCol = Some("cat"))
    val csvReaderOption2 = io.CSVReadOption(fileName = fileName, indexCol = Some("dog"))
    val csvReaderOption3 = io.CSVReadOption(fileName = fileName, indexCol = Some("mouse"))

    val df1: DataFrame[Any] = csvReader.read(csvReaderOption1)
    val df2: DataFrame[Any] = csvReader.read(csvReaderOption2)
    val df3: DataFrame[Any] = csvReader.read(csvReaderOption3)

    df1.index.index.get("1") should be (Some(0))
    df2.index.index.get("4") should be (Some(0))
    df3.index.index.get("0.1") should be (Some(0))
  }

  "Reading from different data csv" should "return same result" in {
    val csvReadingOption = CSVReadOption(fileName)
    val df = csvReader2.read(csvReadingOption)
    df.data should be(Seq(Seq( 1, "abc"), Seq(4, ""), Seq(0.1, 1)))
  }

  trait MockFileSystem extends GrubIO {
    override def source(fileName: String): Iterator[String] = Seq("cat;dog;mouse;", "1;4;0.1").iterator
  }

  trait MockFileSystem2 extends GrubIO {
    override def source(fileName: String): Iterator[String] =  Seq("cat;dog;mouse;", "1;4;0.1", "abc;;1").iterator
  }
}

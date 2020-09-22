package grub.io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CSVFileReaderTest extends AnyFlatSpec with should.Matchers
{

  "Reading from csv" should "create dataframe" in {
    val csvReader = CSVReader.read("file_test_1.csv")

  }
}

package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.ds.Join.JoinImplicit

class JoinTest   extends AnyFlatSpec with should.Matchers
{

  val country = Seq(Seq(1,2,3,4,5), Seq("USA", "Pakistan", "Germany", "Turkey", "France"))
  val city = Seq(Seq(1,2,3,4,5,6,7), Seq("New York", "Karachi", "Lahore", "Berlin", "Izmir", "Munchen", "California"), Seq(1,2,2,3,4,3,1), Seq(19, 32, 12, 41, 51, 12, 24))

  val countryColumns = List("Id", "Country Name")
  val cityColumns = List("City Id", "City Name", "Country Id", "Population")

  "Inner join" should "give me common dataframe" in {
    val mainDf = DataFrame(country, countryColumns)
    val secDf = DataFrame(city, cityColumns)

    val inner = mainDf.inner(secDf, "Id", "Country Id", (x: Int, y: Int) => x == y)
    val result1 = inner.columns("Country Name")
    result1.data(0) should be (Seq("USA", "USA", "Pakistan", "Pakistan", "Germany", "Germany", "Turkey"))
    inner.locate(0).data should be (Seq(Seq(1), Seq("USA"), Seq(1), Seq("New York"), Seq(1), Seq(19)))
  }

  "Inner join" should "give me greater than dataframe" in {
    val mainDf = DataFrame(country, countryColumns)
    val secDf = DataFrame(city, cityColumns)

    val inner = mainDf.inner(secDf, "Id", "Country Id", (x: Int, y: Int) => x > y)
    inner.data.size should be (6)
    inner.data(0).size should be (9)
  }

  it should "return empty dataframe" in {
    val mainDf = DataFrame(country, countryColumns)
    val secDf = DataFrame(city, cityColumns)

    the [IllegalArgumentException] thrownBy(mainDf.inner(secDf, "Id", "Population", (x: Int, y: Int) => x == y))
  }
}

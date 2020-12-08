package grub.ds

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import grub.ds.Filtering.FilteringImplicit

class FilteringTest extends AnyFlatSpec with should.Matchers
{

  val data = Seq(Seq(1,2,3,4,5,6,7), Seq(1.036,2.202,3.98,4.11,5.6,6.3,7.2), Seq("d1", "d2", "d3", "d4", "d5", "d6", "d7"))
  val columns = Seq("A", "B", "C")
  "Filtering" should "return matching results in function provided" in {
    val df: DataFrame[Any] = DataFrame(data, columns)

    val find: Int = 4;


    val mDf1: DataFrame[Int] = df.asType[Int]("A").filter( x => x == find  )
    val mDf2: DataFrame[Double] =  df.asType[Double]("B").filter( x => x > find  )
    val mDf3: DataFrame[Double] =  df.asType[Double]("B").filter( x => x > find  )

    mDf1.data should be (Seq(Seq(4)))
    mDf2.data should be (Seq(Seq(4.11, 5.6, 6.3, 7.2)))
    mDf3.data should be (Seq(Seq(4.11, 5.6, 6.3, 7.2)))
  }

  "Filtering" should "return fill Na" in {
    val data = Seq(Seq("A", "", null, None), Seq(1,2,3,null))
    val df = DataFrame(data)
    val filled = df.fill(0, List("1"))

    filled.data(1) should be (Seq(1,2,3,0))

    val filled2 = df.fill(0, List("0", "1"))
    filled2.data(0) should be (Seq("A", 0, 0, 0))
    filled2.data(1) should be (Seq(1,2,3,0))


    val filled3 = df.fill("0", List("0"))
    filled3.data(0) should be (Seq("A", "0", "0", "0"))
  }

  "Filtering" should "drop values" in {
    val data = Seq(Seq(1,2,null, 4,3, null))
    val df = DataFrame(data)

    val dropped1 = df.drop(null)
    dropped1.data(0) should be (Seq(1,2,4,3))

    val data1 = Seq(Seq(1,2,null, 3), Seq("A", "B", "C", null))
    val df2 = DataFrame(data1)
    val dropped2 = df2.drop(null)
    dropped2.data should be (Seq(Seq(1,2), Seq("A", "B")))
  }
}
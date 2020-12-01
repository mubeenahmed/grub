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


}

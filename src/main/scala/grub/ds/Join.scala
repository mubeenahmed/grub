package grub.ds

import shapeless.Typeable
import grub.ds.CrudOps.CrudOpsImplicit

/**
 * To merge different dataframe with some common conditions
 */
object Join {

  /**
   * Implicit helpers for [[grub.ds.DataFrame]] instances. This provide the actual implementation for Join function
   * @param dataFrame
   * @tparam V
   */
  implicit class JoinImplicit[V: Typeable](dataFrame: DataFrame[V]) {

    /**
     * Inner join, returns the common elements between the two dataframe
     * @param df
     * @param leftColumn
     * @param rightColumn
     * @param ops
     * @tparam T
     * @return
     */
    def inner[T](df: DataFrame[V], leftColumn: String, rightColumn: String, ops: (T, T) => Boolean): DataFrame[V] = {
      val leftColumnData: Seq[V] = dataFrame.columns(leftColumn).single
      val rightColumnData: Seq[V] = df.columns(rightColumn).single

      val toMerge: Seq[(Int, Int)] =
        joinCondition[T](leftColumnData, rightColumnData, ops).collect {
          case (x, Some(y)) => (x, y)
        }

      val data: Seq[Seq[Seq[V]]] = extractRowData(df, toMerge)

      val t: Seq[Seq[V]] = data.flatMap(x => x.transpose).transpose
      DataFrame(t, dataFrame.columns.all ++ df.columns.all)
    }

    /**
     * Left join, returns the common elements between with uncommon left dataframe column if no common on right
     * @param df
     * @param leftColumn
     * @param rightColumn
     * @param ops
     * @tparam T
     * @return
     */
    def left[T](df: DataFrame[V], leftColumn: String, rightColumn: String, ops: (T, T) => Boolean): DataFrame[V] = {
      val leftColumnData: Seq[V] = dataFrame.columns(leftColumn).single
      val rightColumnData: Seq[V] = df.columns(rightColumn).single

      val toMerge: Seq[(Int, Option[Int])] = joinCondition[T](leftColumnData, rightColumnData, ops)
      val data: Seq[Seq[Seq[V]]] = leftExtractRowData(df, toMerge)

      val t: Seq[Seq[V]] = data.flatMap(x => x.transpose).transpose
      DataFrame(t, dataFrame.columns.all ++ df.columns.all)
    }

    /**
     * Right join, returns the common elements between with uncommon right dataframe column if no common on left
     * @param df
     * @param leftColumn
     * @param rightColumn
     * @param ops
     * @tparam T
     * @return
     */
    def right[T](df: DataFrame[V], leftColumn: String, rightColumn: String, ops: (T, T) => Boolean): DataFrame[V] = {
      val leftColumnData: Seq[V] = dataFrame.columns(leftColumn).single
      val rightColumnData: Seq[V] = df.columns(rightColumn).single

      val toMerge: Seq[(Int, Option[Int])] = joinCondition[T](rightColumnData, leftColumnData, ops)
      val data: Seq[Seq[Seq[V]]] = rightExtractRowData(df, toMerge)

      val t: Seq[Seq[V]] = data.flatMap(x => x.transpose).transpose
      DataFrame(t, dataFrame.columns.all ++ df.columns.all)
    }


    private def joinCondition[T](leftColumnData: Seq[V], rightColumnData: Seq[V], ops: (T, T) => Boolean) = {
      leftColumnData.zipWithIndex.collect { left =>
        val right = rightColumnData.zipWithIndex
          .filter(x => ops(x._1.asInstanceOf[T], left._1.asInstanceOf[T]))
        if(right.size > 0) {
          for {
            r <- right
          } yield (left._2, Some(r._2))
        }
        else {
          Seq((left._2, None))
        }
      }.flatten
    }


    private def extractRowData(df: DataFrame[V], toMerge: Seq[(Int, Int)]) = {
      val data: Seq[Seq[Seq[V]]] = for {
        (left, right) <- toMerge
        row = mergeDataFrame(left, Some(right), df)
      } yield row
      data
    }

    private def leftExtractRowData(df: DataFrame[V], toMerge: Seq[(Int, Option[Int])]) = {
      val data: Seq[Seq[Seq[V]]] = for {
        (left, right) <- toMerge
        row = mergeDataFrame(left, right, df)
      } yield row
      data
    }

    private def rightExtractRowData(df: DataFrame[V], toMerge: Seq[(Int, Option[Int])]) = {
      val data: Seq[Seq[Seq[V]]] = for {
        (left, right) <- toMerge
        row = mergeDataFrame(left, right, this.dataFrame, df)
      } yield row
      data
    }

    private def mergeDataFrame(left: Int, right: Option[Int], df: DataFrame[V], dataFrame: DataFrame[V] = this.dataFrame): Seq[Seq[V]] = {
      val leftData: Seq[Seq[V]] = dataFrame.locateRow(left).data
      if(right.isDefined) {
        val rightData: Seq[Seq[V]] = df.locateRow(right.get).data
        leftData ++ rightData
      }
      else {
        leftData ++ LazyList.range(0, df.data.size).zipWithIndex.map(_ => Seq(None.asInstanceOf[V]))
      }
    }

  }

}

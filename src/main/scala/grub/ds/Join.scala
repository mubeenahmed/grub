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

      val toMerge: Seq[(Int, Int)] = joinCondition[T](leftColumnData, rightColumnData, ops)
      val data: Seq[Seq[Seq[V]]] = extractRowData(df, toMerge)

      val t: Seq[Seq[V]] = data.flatMap(x => x.transpose).transpose
      DataFrame(t, dataFrame.columns.all ++ df.columns.all)
    }

    private def extractRowData(df: DataFrame[V], toMerge: Seq[(Int, Int)]) = {
      val data: Seq[Seq[Seq[V]]] = for {
        (left, right) <- toMerge
        row = mergeDataFrame(left, right, df)
      } yield row
      data
    }

    private def joinCondition[T](leftColumnData: Seq[V], rightColumnData: Seq[V], ops: (T, T) => Boolean) = {
      val toMerge: Seq[(Int, Int)] = for {
        left <- leftColumnData.zipWithIndex
        right <- rightColumnData.zipWithIndex.filter(x => ops(x._1.asInstanceOf[T], left._1.asInstanceOf[T]))
      } yield (left._2, right._2)
      toMerge
    }

    private def mergeDataFrame(left: Int, right: Int, df: DataFrame[V]): Seq[Seq[V]] = {
      val leftData: Seq[Seq[V]] = dataFrame.locateRow(left).data
      val rightData: Seq[Seq[V]] = df.locateRow(right).data

      leftData ++ rightData
    }

  }

}

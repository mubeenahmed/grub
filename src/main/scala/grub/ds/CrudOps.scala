package grub.ds

import shapeless.Typeable

object CrudOps {

  implicit class CrudOpsImplicit[V: Typeable](dataFrame: DataFrame[V]) {

    def addCol(name: String, data: Seq[V]): DataFrame[V] = {
      val dset = dataFrame.data :+ data
      val columns = dataFrame.columns.all :+ name
      DataFrame(dset, columns, dataFrame.index)
    }

    def addRow(row: Seq[V]): DataFrame[V] =
      if(row.size != dataFrame.data(0).size) throw new IllegalArgumentException("Adding row is unequal")
      else newRow(row)

    private def newRow(row: Seq[V]): DataFrame[V] = {
      val newData = for {
        ri <- row.zipWithIndex
        newRowAdded = if(dataFrame.data.size > 0) dataFrame.data(ri._2) :+ ri._1 else Seq() :+ ri._1
      } yield newRowAdded

      val addingIndex = dataFrame.index.index.size + 1

      if(dataFrame.data.size > 0)
        dataFrame.index.index.put(addingIndex, dataFrame.data(0).size)
      DataFrame(newData, dataFrame.columns.all, dataFrame.index)
    }

    def deleteRow[T](index: T): DataFrame[V] = {
      val i = dataFrame.index.get(index)
      val newRows: Seq[Seq[V]] = for {
        cols <- dataFrame.data
        newRow = cols.zipWithIndex.filter(x => x._2 != i).map(x => x._1)
      } yield newRow

      dataFrame.index.index.remove(index)
      val columns = dataFrame.columns.all

      DataFrame(newRows, columns, dataFrame.index)
    }

    def deleteColumn(colName: String): DataFrame[V] = {
      val index = dataFrame.columns.get(colName)
      val withRemovedRow: Seq[Seq[V]] = dataFrame.data
        .zipWithIndex
        .filter(x => x._2 != index)
        .map(x => x._1)

      val columns = dataFrame.columns.all.filter(x => x != colName)
      DataFrame(withRemovedRow, columns, dataFrame.index)
    }

    def append(df: DataFrame[V]): DataFrame[V] = {
      if(df.columns.all.size != dataFrame.columns.all.size) {
        throw new IllegalArgumentException("DataFrame found uneven")
      }
      val appends: Seq[Seq[V]] = for {
        data <- dataFrame.data.zipWithIndex
        append = data._1 ++ df.data(data._2)
      } yield append
      dataFrame.index.index.addAll(df.index.index)
      DataFrame(appends, df.columns.all, dataFrame.index)
    }

  }

}

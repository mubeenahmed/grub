package grub.ds

import grub.ds.CrudOps.CrudOpsImplicit
import shapeless.Typeable

object Filtering {

  implicit class FilteringImplicit[V: Typeable](val dataFrame: DataFrame[V]) {

    def === [T <: V](value: T): DataFrame[V] = {
      val df: Seq[Seq[V]] = execute(x => x == value)
      DataFrame(df, dataFrame.columns.all)
    }

    private def execute(fn: V => Boolean): Seq[Seq[V]] =
      dataFrame.data.map(x => x.filter(fn(_)))
  }
}

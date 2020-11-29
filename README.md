# grub
Dataframe style library for manipulating data, using statistics on data and visualization of data. You can use different sources, such as CSV to parse the data and perform statistics, manipulate, transform and visualize the data. 


The quick and simple ways to create a dataframe

## Understanding, how to create dataframe

To create a empty dataframe, you can provide empty list of list or don't provide any data. For example

```val df = Dataframe()  // This will create an empty dataframe```

To create a dataframe you can only provide a data. Remember one list is a column, for exampe `Seq(Seq(1,2,3)` this is one column with `1,2,3` data, if you want to provide two columns or more than adding more `Seq` will add more columns for example `Seq(Seq(1,2,3), Seq("USA", "Canada", "Australia")`

```
// To create a dataframe with data. 
val df = DataFrame(Seq(Seq(1,2,3), Seq("USA", "Canada", "Australia")))
```

You can add columns name to your data

```
// To create a dataframe with data and column names
val df = DataFrame(Seq(Seq(1,2,3), Seq("USA", "Canada", "Australia")), Seq("ID", "Country"))
```

You can add indexes, there are different type of index, if you need to implement your own index, you can inhert from `Index` trait. The index are used to fetch rows from the dataframe in O(1) times. There are pre-added index such as `DateTimeIndex`, `ArraybasedIndex` and `EmptyIndex`

```
// To create a RangeIndex index 
val df = DataFrame(Seq(Seq(1,2,3), Seq("USA", "Canada", "Australia")), Seq("ID", "Country"), RangeIndex(start = 3, end = 5))
```



## How to perform statistics and do data manipulation

For having behavior to manipluate different data, you have to import some implicits into the context for example, the `BasicStatistic` object should be imported.

```
import grub.ds.BasicStatistic.BasicStatisticImplicits
import grub.ds.BasicStatistic.InformationImplicits

// ... 
df.describe // This will return the dataframe with mean, standard deviation, min, and max information. 
df.mean // This will return the list of means
```

## How to use external datasource such as CSV 

Create a instance of `CSVReader` with the source trait for example

```val csvReader = new CSVReader with FileSystem```

The `FileSystem` is the trait. You can provide your own implementation by extending `GrubIO` trait.




There are other method to manipulate different data in dataframe for example 

1. ```def addRow(row: Seq[V]): DataFrame[V]```
2. ```def addCol(name: String, data: Seq[V]): DataFrame[V]```
3. ```def deleteRow[T](index: T): DataFrame[V]```
4. ```def deleteColumn(colName: String): DataFrame[V]```
5. ```def append(df: DataFrame[V]): DataFrame[V]```
6. ```def === [T <: V](value: T): DataFrame[V]```
7. ```def inner[T](df: DataFrame[V], leftColumn: String, rightColumn: String, ops: (T, T) => Boolean): DataFrame[V]```
8. ```def left[T](df: DataFrame[V], leftColumn: String, rightColumn: String, ops: (T, T) => Boolean): DataFrame[V]```


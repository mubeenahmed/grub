package grub.ds

import org.knowm.xchart.{SwingWrapper, XYChart, XYChartBuilder}
import shapeless.Typeable

object Visualize {
  implicit class VisualizeImplicit[V: Typeable](dataFrame: DataFrame[V]) {

    def createChart(title: String = "Chart 1",
                    xName: String = "X-Axis",
                    yName: String = "Y-Axis",
                    seriesName: String = "f = y(x)",
                    xCol: List[String],
                    yCol: String): XYChart =
    {
      val xData: Array[Array[Double]] = dataFrame.columns(xCol:_*)
        .data.map(x => x.map(y => y.asInstanceOf[Double]).toArray[Double]).toArray

      val yData = dataFrame.columns(yCol).single.map(x => x.asInstanceOf[Double]).toArray

      val chart = new XYChartBuilder().width(600).height(600)
        .title(title).xAxisTitle(xName).yAxisTitle(yName)
        .build()
      xData.zipWithIndex
        .foreach(data => chart.addSeries(s"${seriesName} - ${data._2}", data._1, yData))
      chart
    }

    def displayPlot(title: String = "Chart 1",
                    xName: String = "X-Axis",
                    yName: String = "Y-Axis",
                    seriesName: String = "f = y(x)",
                    xCol: List[String],
                    yCol: String): Unit =
    {
      val chart = createChart(title,xName, yName, seriesName, xCol, yCol)
      new SwingWrapper(chart).displayChart()
      while(true) {}
    }
  }
}

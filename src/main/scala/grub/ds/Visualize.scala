package grub.ds

import org.knowm.xchart.{SwingWrapper, XYChart, XYChartBuilder}
import shapeless.Typeable

/**
 * A class for visualizing data, create the chart and displays using JFrame
 */
object Visualize {
  implicit class VisualizeImplicit[V: Typeable](dataFrame: DataFrame[V]) {

    /**
     * Creates Line Chart
     * @param title
     * @param xName
     * @param yName
     * @param seriesName
     * @param xCol
     * @param yCol
     * @return
     */
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

    /**
     * Displays swing (JFrame) chart
     * @param chart
     */
    def displayPlot(chart: XYChart): Unit =
    {
      new SwingWrapper(chart).displayChart()
    }
  }
}

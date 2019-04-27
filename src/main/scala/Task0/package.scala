import java.io.File

import scopt._

import scala.collection.immutable.Map


package object Task0 {
  def arrayToMap(args: Array[String]): Map[String, String] = {
    val dictionary = args
      .grouped(2)
      .flatMap(x => Map(x(0) -> x(1)))
      .toMap
    dictionary
  }

  def crimesMain(args: Array[String]): Unit = {
    case class Config(dir: String = "")
    val parser = new OptionParser[Config]("crime") {
      head("Crimes Rate", "1.0")
      opt[String]('d', "dir")
        .action((x, c) => c.copy(dir = x))
        .valueName("<data dir>")
        .required()
        .text("dir is a required property")
    }
    val config = parser.parse(args, Config())
    config match {
      case Some(conf) => createReport(conf.dir)
      case _ => println("config undefined")
    }
  }

  def createReport(dir: String): Unit = {
    val file_rows = readFiles(dir)
    val data = file_rows.flatMap(convertToColumns)
    val result = filterData(data)
      .groupBy(x => (x("Latitude"), x("Longitude")))
      .flatMap(x => Map(x._2.size -> x._2))
      .toList
      .sortBy(_._1)
      .reverse
      .take(5)
    result.foreach(printResult)
  }

  def readFiles(dir: String): List[Iterator[String]] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList.map(io.Source.fromFile(_).getLines)
    } else {
      List[Iterator[String]]()
    }
  }

  def convertToColumns(dataSet: Iterator[String]): Iterator[Map[String, String]] = {
    val columns = dataSet.next.split(",")
    dataSet
      .map(line => line.split(",").map(_.trim))
      .map(row => (columns zip row).toMap)
  }

  def filterData(data: List[Map[String, String]]): List[Map[String, String]] = {
    data
      .filter(row => row("Crime ID").trim.length > 0)
      .filter(row => row("Longitude").matches("-?[0-9]+(.[0-9]+)?"))
      .filter(row => row("Latitude").matches("-?[0-9]+(.[0-9]+)?"))
      .filter(row => row("Crime type").contains("theft"))
  }

  def printResult(data: (Int, List[Map[String, String]])): Unit = {
    println(
      s"${List.fill(480)("_").mkString}"
    )
    println(
      s"(${data._2.head("Latitude")} : ${data._2.head("Longitude")}) : ${data._1}"
    )
    println(
      s"${data._2.head.keys.mkString("|")}"
    )
    data._2.map(row => s"${row.values.mkString("|")}").foreach(println)
  }
}



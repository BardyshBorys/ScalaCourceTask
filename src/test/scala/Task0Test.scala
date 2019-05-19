import java.io.ByteArrayOutputStream

import Task0._
import org.scalatest.FunSuite

import scala.io.Source

class Task0Test extends FunSuite {

  test("Task0.readFiles") {
    assert(
      readFiles("src/test/test_data").map(_.toList) === List(List("test"))
    )
  }
  test("Task0.arrayToMap") {
    assert(
      arrayToMap(Array("-d", "test_path")) === Map("-d" -> "test_path")
    )
  }

  test("common.convertToColumns") {
    val test_data = List("foo,bar", "1,2").iterator
    assert(
      convertToColumns(test_data).toList === List(Map("foo" -> "1", "bar" -> "2"))
    )
  }

  test("Task0.filterData") {
    val test_data = List(
      Map("Crime ID" -> "test1", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "local theft"),
      Map("Crime ID" -> "", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "theft"),
      Map("Crime ID" -> "test2", "Longitude" -> "No location", "Latitude" -> "2.0", "Crime type" -> "local theft"),
      Map("Crime ID" -> "test3", "Longitude" -> "1.0", "Latitude" -> "", "Crime type" -> "local theft"),
      Map("Crime ID" -> "test4", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "test1"),
      Map("Crime ID" -> "test5", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "local theft")
    )
    assert(
      filterData(test_data) === List(
        Map("Crime ID" -> "test1", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "local theft"),
        Map("Crime ID" -> "test5", "Longitude" -> "1.0", "Latitude" -> "2.0", "Crime type" -> "local theft")
      )
    )
  }

  test("Task0.crimes_main") {
    val acceptable_output = Source.fromFile("src/test/crimes_output.txt").mkString
    val outCapture = new ByteArrayOutputStream
    Console.withOut(outCapture) {
      val test_folder = Array("-d", "src/test/crimes")
      crimesMain(test_folder)
    }
    assert(acceptable_output == outCapture.toString)
  }
}
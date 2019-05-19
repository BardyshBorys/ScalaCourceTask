import Task2._
import org.scalatest.FunSuite

import scala.concurrent.duration._

class Task2Test extends FunSuite {

  test("Task2.retryFuture.recursionDepth") {
    val retries = List(0.seconds, 1.seconds, 2.seconds)
    var recursionDepth = 0
    retryFuture[Int](
      block = () => {
        recursionDepth += 1; 1 + 2
      },
      acceptResult = res => res % 2 == 0,
      retries = retries
    )
    assert(recursionDepth == retries.length + 1)
    recursionDepth = 0
    retryFuture[Int](
      block = () => {
        recursionDepth += 1; 1 + 1
      },
      acceptResult = res => res % 2 == 0,
      retries = retries
    )
    assert(recursionDepth == 1)
  }

  test("Task2.retryFuture.timing") {
    val retries = List(0.seconds, 1.seconds, 2.seconds)
    val max_timing = retries.map(_.toSeconds).sum
    val min_timing = retries.map(_.toSeconds).min
    var t0 = System.nanoTime()
    retryFuture[Int](
      block = () => 1 + 2,
      acceptResult = res => res % 2 == 0,
      retries = retries
    )
    var t1 = System.nanoTime()
    assert(Duration(t1 - t0, NANOSECONDS).toSeconds == max_timing)
    t0 = System.nanoTime()
    retryFuture[Int](
      block = () => 1 + 1,
      acceptResult = res => res % 2 == 0,
      retries = retries
    )
    t1 = System.nanoTime()
    assert(Duration(t1 - t0, NANOSECONDS).toSeconds == min_timing)
  }

}

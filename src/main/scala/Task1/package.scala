import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

package object Task1 {
  @tailrec
  final def retrySimple[A](block: () => A,
                           acceptResult: A => Boolean,
                           retries: List[FiniteDuration]): A = {

    val result = block()
    if (acceptResult(result) | retries.isEmpty) {
      result
    } else {
      Thread.sleep(retries.head.toMillis)
      retrySimple(block, acceptResult, retries.tail)
    }
  }
}




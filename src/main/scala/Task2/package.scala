import scala.annotation.tailrec
import scala.concurrent.duration.{FiniteDuration, _}

package object Task2 {

  @tailrec
  final def retryFuture[A](block: () => A,
                           acceptResult: A => Boolean,
                           retries: List[FiniteDuration]): A = {
    import scala.concurrent._
    import ExecutionContext.Implicits.global
    val future: Future[A] = Future {
      block()
    }
    val result = Await.result(future, 1.seconds)
    print(result)
    if (acceptResult(result) | retries.isEmpty) {
      result
    } else {
      Thread.sleep(retries.head.toMillis)
      retryFuture(block, acceptResult, retries.tail)
    }
  }
}

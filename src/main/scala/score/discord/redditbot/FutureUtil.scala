package score.discord.redditbot

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}

object FutureUtil {
  implicit def toRichFuture[T](me: Future[T]): RichFuture[T] = new RichFuture[T](me)
}

class RichFuture[T](val me: Future[T]) extends AnyVal {
  def asSuccessOption(
    errorReporter: Throwable => Unit = ExecutionContext.defaultReporter
  )(implicit executionContext: ExecutionContext): Future[Option[T]] =
    me.transform {
      case Success(s) => Success(Some(s))
      case Failure(ex) =>
        errorReporter(ex)
        Success(None)
    }
}

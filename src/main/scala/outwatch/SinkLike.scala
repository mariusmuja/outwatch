package outwatch

import monix.execution.Ack.{Continue, Stop}
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import monix.reactive.observers.Subscriber
import org.scalajs.dom.console

import scala.concurrent.Future
import scala.util.control.NonFatal

trait SinkLike[T, S] {
  def toSink(s: S): Sink[T]
}

trait SinkSyntax {
  implicit def toSinkSyntax[T, S](sink: S): SinkLikeOps[T, S] = new SinkLikeOps(sink)

  implicit def toSink[T, S](sink: S)(implicit ev: SinkLike[T, S]): Sink[T] = ev.toSink(sink)
}
object SinkSyntax extends SinkSyntax

final class SinkLikeOps[T, S](private val sink: S) extends AnyVal {
  def toSink(implicit ev: SinkLike[T, S]): Sink[T] = ev.toSink(sink)
}


object SinkLike {
  import SinkSyntax._

  implicit def fromSink[T, S](implicit ev: S <:< Sink[T]): SinkLike[T, S] = (s: S) => s

  implicit def fromObserver[T, S](implicit s: Scheduler, ev: S <:< Observer[T]): SinkLike[T, S] = (o: S) => ObserverSink(Subscriber(o, s))

  implicit def fromSubscriber[T, S](implicit ev: S <:< Subscriber[T]): SinkLike[T, S] = (s: S) => ObserverSink(s)

  type FunctionSink[T] = T => Future[Ack]
  implicit def fromFunction[T, S](implicit s: Scheduler, ev: S <:< FunctionSink[T]): SinkLike[T, S] = (f: S) => {
    val observer = new Observer[T] {
      def onNext(elem: T): Future[Ack] = {
        try {
          f(elem)
        } catch {
          case NonFatal(ex) =>
            onError(ex)
            Stop
        }
      }
      def onError(ex: Throwable): Unit = console.error(ex.getMessage, ex.getStackTrace.mkString("\n"))
      def onComplete(): Unit = ()
    }
    ObserverSink(Subscriber(observer, s))
  }

  type EffectSink = () => Unit
  implicit def fromEffectFunction(implicit s: Scheduler): SinkLike[Any, EffectSink] = f => { _: Any => f(); Continue }

}
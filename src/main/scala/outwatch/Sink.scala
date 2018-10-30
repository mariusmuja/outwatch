package outwatch

import monix.execution.Ack.Stop
import monix.execution.misc.NonFatal
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.{Observer, Pipe => MonixPipe}
import monix.reactive.observers.{SafeSubscriber, Subscriber}
import monix.reactive.subjects.PublishSubject
import outwatch.dom.{IO, Observable}

import scala.concurrent.Future


sealed trait Sink[-T] extends Any {

  /**
    * Use this function with caution!
    * This function pipes all of the Observable's emissions into this Sink
    * Using this method is inherently impure and can cause memory leaks, if subscription
    * isn't handled correctly. For more guaranteed safety, use Sink.redirect() instead.
    */
  def <--(observable: Observable[T]): IO[Cancelable] = IO {
    observable.subscribe(observer)
  }

  private[outwatch] def observer: Subscriber[T]

  def unsafeOnNext(value: T): Future[Ack] = observer.onNext(value)

  def feed(values: Iterator[T]): Future[Ack] = observer.feed(values)

  def feed(values: Iterable[T]): Future[Ack] = observer.feed(values)

  /**
    * Creates a new sink. That sink will transform the values it receives and then forward them along to this sink.
    * The transformation is described by a function from an Observable to another Observable, i.e. an operator on Observable.
    * This function applies the operator to the newly created sink and forwards the value to the original sink.
    * @param projection the operator to use
    * @tparam R the type of the resulting sink
    * @return the resulting sink, that will forward the values
    */
  def redirect[R](projection: Observable[R] => Observable[T]): Sink[R] = {
    Sink.redirect(this)(projection)
  }

  /**
    * Creates a new sink. That sink will transform each value it receives and then forward it along to the this sink.
    * The transformation is a simple map from one type to another, i.e. a 'map'.
    * This is equivalent to `contramap` on a `Contravariant` functor, since `Sink`s are contravariant in nature.
    * @param projection the mapping to perform before forwarding
    * @tparam R the type of the resulting sink
    * @return the resulting sink, that will forward the values
    */
  def redirectMap[R](projection: R => T): Sink[R] = {
    Sink.redirectMap(this)(projection)
  }
}

object Sink {

  private[outwatch] final case class ObservableSink[-I, +O](oldSink: Sink[I], stream: Observable[O]) extends Observable[O] with Sink[I] {
    override private[outwatch] def observer = oldSink.observer

    override def unsafeSubscribeFn(subscriber: Subscriber[O]): Cancelable = stream.unsafeSubscribeFn(subscriber)
  }

  private[outwatch] final case class SubjectSink[T]()(implicit scheduler: Scheduler) extends Observable[T] with Sink[T] {
    private val subject = PublishSubject[T]

    override private[outwatch] def observer = SafeSubscriber(Subscriber(subject, scheduler))

    override def unsafeSubscribeFn(subscriber: Subscriber[T]): Cancelable = subject.unsafeSubscribeFn(subscriber)
  }

  /**
    * Creates a new Sink from Scratch.
    * This function takes another function as its parameter that will be executed every time the Sink receives an emitted value.
    *
    * @param next the function to be executed on every emission
    * @param error the function to be executed on error
    * @param complete the function to be executed on completion
    * @tparam T the type parameter of the consumed elements.
    * @return a Sink that consumes elements of type T.
    */
  def create[T](next: T => Future[Ack],
                error: Throwable => Unit = _ => (),
                complete: () => Unit = () => ()
               )(implicit s: Scheduler): IO[Sink[T]] = {
    IO {
      ObserverSink(
        new Observer[T] {
          override def onNext(t: T): Future[Ack] = next(t)
          override def onError(ex: Throwable): Unit = error(ex)
          override def onComplete(): Unit = complete()
        }
      )
    }
  }

  /**
    * Creates a new sink. This sink will transform the values it receives and then forward them along to the passed sink.
    * The transformation is described by a function from an Observable to another Observable, i.e. an operator on Observable.
    * This function applies the operator to the newly created sink and forwards the value to the original sink.
    * @param sink the Sink to forward values to
    * @param project the operator to use
    * @tparam R the type of the resulting sink
    * @tparam T the type of the original sink
    * @return the resulting sink, that will forward the values
    */
  def redirect[T,R](sink: Sink[T])(project: Observable[R] => Observable[T]): Sink[R] = {
    implicit val scheduler = sink.observer.scheduler

    val (input, output) = MonixPipe.publish[R].transform(project).multicast
    output.unsafeSubscribeFn(sink.observer)

    ObserverSink(input)
  }

  private class ContramapObserver[R, T](obs: Observer[T])(f: R => T) extends Observer[R] {
    override def onNext(elem: R): Future[Ack] = {
      var streamError = true
      try {
        val a = f(elem)
        streamError = false
        obs.onNext(a)
      } catch {
        case NonFatal(ex) if streamError =>
          onError(ex)
          Stop
      }
    }

    override def onError(ex: Throwable): Unit = obs.onError(ex)

    override def onComplete(): Unit = obs.onComplete()
  }


  /**
    * Creates a new sink. This sink will transform each value it receives and then forward it along to the passed sink.
    * The transformation is a simple map from one type to another, i.e. a 'map'.
    * This is equivalent to `contramap` on a `Contravariant` functor, since `Sink`s are contravariant in nature.
    * @param sink the Sink to forward values to
    * @param f the mapping to perform before forwarding
    * @tparam R the type of the resulting sink
    * @tparam T the type of the original sink
    * @return the resulting sink, that will forward the values
    */
  def redirectMap[T, R](sink: Sink[T])(f: R => T): Sink[R] = {
    implicit val scheduler = sink.observer.scheduler
    ObserverSink(new ContramapObserver(sink.observer)(f))
  }

}

final case class ObserverSink[-T](obs: Observer[T])(implicit s: Scheduler) extends Sink[T] {
  override val observer = Subscriber(obs, s)
}

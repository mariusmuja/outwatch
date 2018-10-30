package outwatch

import monix.execution.Ack.Continue
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import org.scalajs.dom.console

import scala.concurrent.Future



object SideEffects {
  def sinkFromFunction[T](f: T => Unit)(implicit s: Scheduler): Sink[T] = ObserverSink(
    new Observer[T] {
      override def onNext(elem: T): Future[Ack] = { f(elem); Continue }
      override def onError(ex: Throwable): Unit = console.error(ex.getMessage, ex.getStackTrace.mkString("\n"))
      override def onComplete(): Unit = ()
    }
  )
}

trait SideEffects {

  def sideEffect[T](f: T => Unit)(implicit s: Scheduler): Sink[T] = SideEffects.sinkFromFunction(f)
  def sideEffect[S, T](f: (S, T) => Unit)(implicit s: Scheduler): Sink[(S, T)] = SideEffects.sinkFromFunction(f.tupled)
  def sideEffect(f: => Unit)(implicit s: Scheduler): Sink[Any] = SideEffects.sinkFromFunction(_ => f)
}

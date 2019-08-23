package outwatch

import monix.execution.Scheduler
import SinkSyntax._
import monix.execution.Ack.Continue


trait SideEffects {

  def sideEffect[T](f: T => Unit)(implicit s: Scheduler): Sink[T] = { e: T => f(e); Continue }

  def sideEffect[S, T](f: (S, T) => Unit)(implicit s: Scheduler): Sink[(S, T)] = { e: (S, T) => f.tupled(e); Continue }

  def sideEffect(f: => Unit)(implicit s: Scheduler): Sink[Any] = () => f
}

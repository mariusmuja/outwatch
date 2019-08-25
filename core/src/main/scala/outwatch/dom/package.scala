package outwatch

import scala.collection.mutable.ArrayBuffer


package object dom extends Implicits with ManagedSubscriptions with SideEffects {



  type VNode = IO[VTree]
  type VDomModifier = IO[Modifier]

  object VDomModifier {
    val empty: VDomModifier = IO.pure(EmptyModifier)

    def async[T: AsVDomModifier, U: AsVDomModifier](initial: => U = empty)(value: IO[T]): VDomModifier = {
      Observable.fromTask(value).map(AsVDomModifier[T].asVDomModifier).prepend(AsVDomModifier[U].asVDomModifier(initial))
    }

    def sync[T: AsVDomModifier](value: IO[T]): VDomModifier = value.flatMap(AsVDomModifier[T].asVDomModifier)

    def apply(modifier: VDomModifier, modifier2: VDomModifier, modifiers: VDomModifier*): VDomModifier = {
      IO.sequence((ArrayBuffer(modifier, modifier2) ++ modifiers)).map(CompositeModifier)
    }

    def apply[T: AsVDomModifier](t: T): VDomModifier = t
  }

  type Observable[+A] = monix.reactive.Observable[A]
  val Observable = monix.reactive.Observable

  type Sink[-A] = outwatch.Sink[A]
  val Sink = outwatch.Sink

  type Pipe[-I, +O] = outwatch.Pipe[I, O]
  val Pipe = outwatch.Pipe

  type IO[+T] = monix.eval.Task[T]
  val IO = monix.eval.Task
}

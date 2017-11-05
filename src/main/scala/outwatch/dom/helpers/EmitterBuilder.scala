package outwatch.dom.helpers

import cats.effect.IO
import monix.reactive.{Observable, Observer}
import org.scalajs.dom._
import outwatch.Sink
import outwatch.dom.{BoolEventEmitter, NumberEventEmitter, StringEventEmitter, _}

final case class GenericMappedEmitterBuilder[T,E](constructor: Observer[E] => Emitter, mapping: E => T){
  def -->[U >: T](sink: Sink[U]): IO[Emitter] = {
    IO.pure(constructor(sink.redirectMap(mapping).observer))
  }
}

final case class FilteredGenericMappedEmitterBuilder[T,E](
  constructor: Observer[E] => Emitter,
  mapping: E => T,
  predicate: E => Boolean
) {
  def -->[U >: T](sink: Sink[U]): IO[Emitter] = {
    IO.pure(constructor(sink.redirect[E](_.filter(predicate).map(mapping)).observer))
  }
}

final case class WithLatestFromEmitterBuilder[T, E <: Event](eventType: String, stream: Observable[T]) {
  def -->[U >: T](sink: Sink[U]): IO[EventEmitter[E]] = {
    val proxy: Sink[E] = sink.redirect[E](_.withLatestFrom(stream)((_,u) => u))
    IO.pure(EventEmitter(eventType, proxy.observer))
  }
}

final case class FilteredWithLatestFromEmitterBuilder[T, E <: Event](
  eventType: String,
  stream: Observable[T],
  predicate: E => Boolean
) {
  def -->[U >: T](sink: Sink[U]): IO[EventEmitter[E]] = {
    val proxy: Sink[E] = sink.redirect(_.filter(predicate).withLatestFrom(stream)((_, u) => u))
    IO.pure(EventEmitter(eventType, proxy.observer))
  }
}

final case class FilteredEmitterBuilder[E](eventType: String, predicate: E => Boolean) {
  def -->(sink: Sink[E]) =
    IO.pure(EventEmitter(eventType, sink.redirect[E](_.filter(predicate)).observer))

  def apply[T](t: T) =
    FilteredGenericMappedEmitterBuilder(EventEmitter(eventType, _:Observer[E]), (_: E) => t, predicate)

  def apply[T](f: E => T) =
    FilteredGenericMappedEmitterBuilder(EventEmitter(eventType, _:Observer[E]), f, predicate)

  def apply[T](ts: Observable[T]) = FilteredWithLatestFromEmitterBuilder(eventType, ts, predicate)
}

final class EventEmitterBuilder[E <: Event](val eventType: String) extends AnyVal {
  def -->(sink: Sink[E]) =
    IO.pure(EventEmitter(eventType, sink.observer))

  def apply[T](t: T) =
    GenericMappedEmitterBuilder(EventEmitter(eventType, _:Observer[E]), (_: E) => t)

  def apply[T](f: E => T) =
    GenericMappedEmitterBuilder(EventEmitter(eventType, _:Observer[E]), f)

  def apply[T](ts: Observable[T]) = WithLatestFromEmitterBuilder(eventType, ts)

  def filter(predicate: E => Boolean) = FilteredEmitterBuilder(eventType, predicate)
}

final class StringEventEmitterBuilder(val eventType: String) extends AnyVal {
  def -->(sink: Sink[String]) =
    IO.pure(StringEventEmitter(eventType, sink.observer))

  def apply[T](f: String => T) =
    GenericMappedEmitterBuilder(StringEventEmitter(eventType, _: Observer[String]), f)
}

final class BoolEventEmitterBuilder(val eventType: String) extends AnyVal {
  def -->(sink: Sink[Boolean]) =
    IO.pure(BoolEventEmitter(eventType, sink.observer))

  def apply[T](f: Boolean => T) =
    GenericMappedEmitterBuilder(BoolEventEmitter(eventType, _: Observer[Boolean]), f)
}

final class NumberEventEmitterBuilder(val eventType: String) extends AnyVal {
  def -->(sink: Sink[Double]) =
    IO.pure(NumberEventEmitter(eventType, sink.observer))

  def apply[T](f: Double => T) =
    GenericMappedEmitterBuilder(NumberEventEmitter(eventType, _: Observer[Double]), f)
}

object InsertHookBuilder {
  def -->(sink: Sink[Element]) = IO.pure(InsertHook(sink.observer))
}

object DestroyHookBuilder {
  def -->(sink: Sink[Element]) = IO.pure(DestroyHook(sink.observer))
}

object UpdateHookBuilder {
  def -->(sink: Sink[(Element, Element)]) = IO.pure(UpdateHook(sink.observer))
}

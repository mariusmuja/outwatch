package outwatch.dom.helpers

import monix.reactive.Observer
import org.scalajs.dom.Event
import outwatch.Sink
import outwatch.all.VDomModifier
import outwatch.dom.{Emitter, IO, Modifier, Observable}


trait EmitterBuilder[O, R] extends Any {

  def transform[T](tr: Observable[O] => Observable[T]): EmitterBuilder[T, R]

  def -->(sink: Sink[O]): R

  @inline final def apply[T](value: => T): EmitterBuilder[T, R] = mapTo(value)

  @inline final def mapTo[T](value: => T): EmitterBuilder[T, R] = map(_ => value)

  @inline final def apply[T](latest: Observable[T]): EmitterBuilder[T, R] = transform(_.withLatestFrom(latest)((_, u) => u))

  @inline final def map[T](f: O => T): EmitterBuilder[T, R] = transform(_.map(f))

  @inline final def filter(predicate: O => Boolean): EmitterBuilder[O, R] = transform(_.filter(predicate))

  @inline final def collect[T](f: PartialFunction[O, T]): EmitterBuilder[T, R] = transform(_.collect(f))
}

final case class TransformingEmitterBuilder[E, O, R] private[helpers](
  transformer: Observable[E] => Observable[O],
  create: Observer[E] => R
) extends EmitterBuilder[O, R] {

  def transform[T](tr: Observable[O] => Observable[T]): EmitterBuilder[T, R] = copy(
    transformer = tr compose transformer
  )

  def -->(sink: Sink[O]): R = {
    val redirected: Sink[E] = sink.redirect[E](transformer)
    create(redirected.subscriber)
  }
}

final case class SimpleEmitterBuilder[E, R](create: Observer[E] => R) extends AnyVal with EmitterBuilder[E, R] {

  def transform[T](tr: Observable[E] => Observable[T]): EmitterBuilder[T, R] =
    new TransformingEmitterBuilder[E, T, R](tr, create)

  def -->(sink: Sink[E]): R = create(sink.subscriber)
}

object EmitterBuilder extends EmitterOps {

  private[outwatch] def event[E <: Event](eventType: String): SimpleEmitterBuilder[E, VDomModifier] =
    modifier(obs => Emitter(eventType, { event => obs.onNext(event.asInstanceOf[E]); ()}))


  private[outwatch] def modifier[E](f: Observer[E] => Modifier): SimpleEmitterBuilder[E, VDomModifier] =
    SimpleEmitterBuilder(obs => IO.pure(f(obs)))


  def apply[E](f: Observer[E] => VDomModifier): SimpleEmitterBuilder[E, VDomModifier] =
    SimpleEmitterBuilder(f)
}

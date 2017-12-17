package outwatch.dom.helpers

import cats.effect.IO
import com.raquo.domtypes.jsdom.defs.events.TypedTargetEvent
import org.scalajs.dom._
import outwatch.Sink
import outwatch.dom.{DestroyHook, Emitter, Hook, InsertHook, Observable, PostPatchHook, PrePatchHook, TagWithChecked, TagWithNumber, TagWithString, UpdateHook}


trait EmitterBuilder[E <: Event, O] extends Any with CurrentTargetOps[E, O] {

  private[outwatch] def transform[T](tr: Observable[O] => Observable[T]): TransformingEmitterBuilder[E, T]

  def apply[T](value: T): TransformingEmitterBuilder[E, T] = map(_ => value)

  def apply[T](latest: Observable[T]): TransformingEmitterBuilder[E, T] = transform(_.withLatestFrom(latest)((_, u) => u))

  @deprecated("Deprecated, use '.map' instead", "0.11.0")
  def apply[T](f: O => T): TransformingEmitterBuilder[E, T] = map(f)

  def map[T](f: O => T): TransformingEmitterBuilder[E, T] = transform(_.map(f))

  def filter(predicate: O => Boolean): TransformingEmitterBuilder[E, O] = transform(_.filter(predicate))

  def collect[T](f: PartialFunction[O, T]): TransformingEmitterBuilder[E, T] = transform(_.collect(f))

  def -->(sink: Sink[_ >: O]): IO[Emitter]
}

object EmitterBuilder extends TypedTargetOps {
  def apply[E <: Event](eventType: String) = new SimpleEmitterBuilder[E](eventType)
}

trait TypedTargetOps {

  trait TypedTarget[+Elem <: EventTarget, -E <: Event] {
    def target(event: E): Elem
  }

  trait TypedTargetLowPriority {
    implicit def castEventTarget[Elem <: EventTarget, E <: Event] = new TypedTarget[Elem, E] {
      def target(event: E): Elem = event.target.asInstanceOf[Elem]
    }
  }

  object TypedTarget extends TypedTargetLowPriority {
    implicit def typedTargetEventTarget[Elem <: EventTarget, E <: Event](implicit ev: E <:< TypedTargetEvent[Elem]) =
      new TypedTarget[Elem, E] {
        def target(event: E): Elem = ev(event).target
      }
  }

  class TargetOps[E <: Event, O <: Event, Elem <: EventTarget](event: EmitterBuilder[E, O], getTarget: O => Elem) {
    def value(implicit tag: TagWithString[Elem]): EmitterBuilder[E, String] =
      event.map(ev => tag.value(getTarget(ev)))

    def valueAsNumber(implicit tag: TagWithNumber[Elem]): EmitterBuilder[E, Double] =
      event.map(ev => tag.valueAsNumber(getTarget(ev)))

    def checked(implicit tag: TagWithChecked[Elem]): EmitterBuilder[E, Boolean] =
      event.map(ev => tag.checked(getTarget(ev)))
  }

  implicit class WithTarget[E <: Event, O <: Event](private val builder: EmitterBuilder[E, O]) {
    def target[Elem <: EventTarget](implicit ev: TypedTarget[Elem, O]) = new TargetOps[E, O, Elem](builder, event => ev.target(event))
  }
}

trait CurrentTargetOps[E <: Event, O] extends Any { self: EmitterBuilder[E, O] =>

  def value[Elem <: EventTarget](implicit tag: TagWithString[Elem], ev: O <:< Event): EmitterBuilder[E, String] =
    map(e => tag.value(ev(e).currentTarget.asInstanceOf[Elem]))

  def valueAsNumber[Elem <: EventTarget](implicit tag: TagWithNumber[Elem], ev: O <:< Event): EmitterBuilder[E, Double] =
    map(e => tag.valueAsNumber(ev(e).currentTarget.asInstanceOf[Elem]))

  def checked[Elem <: EventTarget](implicit tag: TagWithChecked[Elem], ev: O <:< Event): EmitterBuilder[E, Boolean] =
    map(e => tag.checked(ev(e).currentTarget.asInstanceOf[Elem]))
}


final case class TransformingEmitterBuilder[E <: Event, O] private[helpers] (
  eventType: String,
  transformer: Observable[E] => Observable[O]
) extends EmitterBuilder[E, O] {

  private[outwatch] def transform[T](tr: Observable[O] => Observable[T]): TransformingEmitterBuilder[E, T] = copy(
    transformer = tr compose transformer
  )

  def -->(sink: Sink[_ >: O]): IO[Emitter] = {
    val observer = sink.redirect(transformer).observer
    IO.pure(Emitter(eventType, event => observer.onNext(event.asInstanceOf[E])))
  }
}


final class SimpleEmitterBuilder[E <: Event] private[helpers]( val eventType: String) extends AnyVal
                                                                                              with EmitterBuilder[E, E] {

  private[outwatch] def transform[O](transformer: Observable[E] => Observable[O]) =
    new TransformingEmitterBuilder[E, O](eventType, transformer)

  def -->(sink: Sink[_ >: E]): IO[Emitter] = IO.pure {
    Emitter(eventType, event => sink.observer.onNext(event.asInstanceOf[E]))
  }
}

trait HookBuilder[E, H <: Hook[_]] {
  def hook(sink: Sink[E]): H

  def -->(sink: Sink[E]): IO[H] = IO.pure(hook(sink))
}

object InsertHookBuilder extends HookBuilder[Element, InsertHook] {
  def hook(sink: Sink[Element]) = InsertHook(sink.observer)
}

object PrePatchHookBuilder extends HookBuilder[(Option[Element], Option[Element]), PrePatchHook] {
  def hook(sink: Sink[(Option[Element], Option[Element])]) = PrePatchHook(sink.observer)
}

object UpdateHookBuilder extends HookBuilder[(Element, Element), UpdateHook] {
  def hook(sink: Sink[(Element, Element)]) = UpdateHook(sink.observer)
}

object PostPatchHookBuilder extends HookBuilder[(Element, Element), PostPatchHook] {
  def hook(sink: Sink[(Element, Element)]) = PostPatchHook(sink.observer)
}

object DestroyHookBuilder extends HookBuilder[Element, DestroyHook] {
  def hook(sink: Sink[Element]) = DestroyHook(sink.observer)
}

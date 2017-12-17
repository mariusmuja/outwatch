package outwatch.dom.helpers

import com.raquo.domtypes.jsdom.defs.events.TypedTargetEvent
import org.scalajs.dom.{Event, EventTarget}

trait TargetOps {

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

  class TypedTargetOps[E <: Event, O <: Event, Elem <: EventTarget](event: EmitterBuilder[E, O], getTarget: O => Elem) {
    def value(implicit tag: TagWithString[Elem]): EmitterBuilder[E, String] =
      event.map(ev => tag.value(getTarget(ev)))

    def valueAsNumber(implicit tag: TagWithNumber[Elem]): EmitterBuilder[E, Double] =
      event.map(ev => tag.valueAsNumber(getTarget(ev)))

    def checked(implicit tag: TagWithChecked[Elem]): EmitterBuilder[E, Boolean] =
      event.map(ev => tag.checked(getTarget(ev)))
  }

  implicit class WithTarget[E <: Event, O <: Event](private val builder: EmitterBuilder[E, O]) {
    def target[Elem <: EventTarget](implicit ev: TypedTarget[Elem, O]) = new TypedTargetOps[E, O, Elem](builder, event => ev.target(event))
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

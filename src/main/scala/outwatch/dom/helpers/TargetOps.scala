package outwatch.dom.helpers

import org.scalajs.dom.{Event, EventTarget, html}

trait TargetOps {

  implicit class EventTargetAsInput[E <: Event, ET <: EventTarget](builder: EmitterBuilder[E, ET]) {
    def value: EmitterBuilder[E, String] = builder.map(_.asInstanceOf[html.Input].value)

    def valueAsNumber: EmitterBuilder[E, Int] = builder.map(_.asInstanceOf[html.Input].valueAsNumber)

    def checked: EmitterBuilder[E, Boolean] = builder.map(_.asInstanceOf[html.Input].checked)
  }

  implicit class CurrentTargetAsInput[E <: Event, O <: Event](builder: EmitterBuilder[E, O]) {
    def value: EmitterBuilder[E, String] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].value)

    def valueAsNumber: EmitterBuilder[E, Int] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].valueAsNumber)

    def checked: EmitterBuilder[E, Boolean] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].checked)
  }

  implicit class TargetMap[E <: Event, O <: Event](builder: EmitterBuilder[E, O]) {
    def target: EmitterBuilder[E, EventTarget] = builder.map(_.target)
  }

}

package outwatch.dom.helpers

import org.scalajs.dom.{Element, Event, html, svg}
import outwatch.all.VDomModifier

trait EmitterOps {

  implicit class EventActions[E <: Event, R](builder: EmitterBuilder[E, R]) {
    def preventDefault: EmitterBuilder[E, R] = builder.map { e => e.preventDefault; e }
    def stopPropagation: EmitterBuilder[E, R] = builder.map { e => e.stopPropagation; e }
    def stopImmediatePropagation: EmitterBuilder[E, R] = builder.map { e => e.stopImmediatePropagation; e }
  }

  implicit class TargetAsInput[E <: Event](builder: EmitterBuilder[E, VDomModifier]) {

    object target {
      def value: EmitterBuilder[String, VDomModifier] = builder.map(_.target.asInstanceOf[html.Input].value)

      def valueAsNumber: EmitterBuilder[Double, VDomModifier] = builder.map(_.target.asInstanceOf[html.Input].valueAsNumber)

      def checked: EmitterBuilder[Boolean, VDomModifier] = builder.map(_.target.asInstanceOf[html.Input].checked)
    }

    def value: EmitterBuilder[String, VDomModifier] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].value)

    def valueAsNumber: EmitterBuilder[Double, VDomModifier] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].valueAsNumber)

    def checked: EmitterBuilder[Boolean, VDomModifier] = builder.map(e => e.currentTarget.asInstanceOf[html.Input].checked)

  }

  implicit class TypedElements[E <: Element](builder: EmitterBuilder[E, VDomModifier]) {
    def asHtml: EmitterBuilder[html.Element, VDomModifier] = builder.map(_.asInstanceOf[html.Element])

    def asSvg: EmitterBuilder[svg.Element, VDomModifier] = builder.map(_.asInstanceOf[svg.Element])
  }

  implicit class TypedElementTuples[E <: Element](builder: EmitterBuilder[(E,E), VDomModifier]) {
    def asHtml: EmitterBuilder[(html.Element, html.Element), VDomModifier] = builder.map(_.asInstanceOf[(html.Element, html.Element)])

    def asSvg: EmitterBuilder[(svg.Element, svg.Element), VDomModifier] = builder.map(_.asInstanceOf[(svg.Element, svg.Element)])
  }
}

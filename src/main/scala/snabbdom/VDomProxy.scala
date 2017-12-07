package snabbdom

import org.scalajs.dom
import outwatch.dom.{AccumAttr, Attr, Attribute, Emitter, EmptyAttribute, Prop, Style}

import scala.scalajs.js

object VDomProxy {

  import js.JSConverters._

  def attrsToSnabbDom(attributes: Seq[Attribute]): (js.Dictionary[Attr.Value], js.Dictionary[Prop.Value], js.Dictionary[String]) = {
    val attrsDict = js.Dictionary[Attr.Value]()
    val propsDict = js.Dictionary[Prop.Value]()
    val styleDict = js.Dictionary[String]()

    attributes.foreach {
      case a: AccumAttr => attrsDict(a.title) = attrsDict.get(a.title).map(a.accum(_, a.value)).getOrElse(a.value)
      case a: Attr => attrsDict(a.title) = a.value
      case a: Prop => propsDict(a.title) = a.value
      case a: Style => styleDict(a.title) = a.value
      case EmptyAttribute =>
    }

    (attrsDict, propsDict, styleDict)
  }


  def emittersToSnabbDom(eventEmitters: Seq[Emitter]): js.Dictionary[js.Function1[dom.Event,Unit]] = {
    eventEmitters
      .groupBy(_.eventType)
      .mapValues(emittersToFunction)
      .toJSDictionary
  }

  private def emittersToFunction(emitters: Seq[Emitter]): js.Function1[dom.Event, Unit] = {
    (event: dom.Event) => emitters.foreach(_.trigger(event))
  }
}

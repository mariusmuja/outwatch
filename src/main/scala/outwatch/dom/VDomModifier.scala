package outwatch.dom

import cats.effect.IO
import monix.reactive.Observer
import org.scalajs.dom._
import outwatch.dom.helpers.DomUtils

import snabbdom.{DataObject, VNodeProxy, hFunction}

import scala.scalajs.js
import collection.breakOut

sealed trait VDomModifier_ extends Any

case class CompositeVDomModifier(modifiers: Seq[VDomModifier]) extends VDomModifier_

case class Emitter(eventType: String, trigger: Event => Unit) extends VDomModifier_

sealed trait Property extends VDomModifier_

sealed trait Attribute extends Property {
  val title: String
}
object Attribute {
  def apply(title: String, value: Attr.Value) = Attr(title, value)
}

final case class Attr(title: String, value: Attr.Value) extends Attribute
object Attr {
  type Value = DataObject.AttrValue
}

/**
  * Attribute that accumulates the previous value in the same VNode with it's value
  */
final case class AccumAttr(title: String, value: Attr.Value, accum: (Attr.Value, Attr.Value)=> Attr.Value) extends Attribute

final case class Prop(title: String, value: Prop.Value) extends Attribute
object Prop {
  type Value = DataObject.PropValue
}

final case class Style(title: String, value: String) extends Attribute

final case class Key(value: Key.Value) extends Property
object Key {
  type Value = DataObject.KeyValue
}

sealed trait Hook extends Property

sealed trait HookSingle extends Hook {
  def observer: Observer[Element]
}

sealed trait HookPair extends Hook {
  def observer: Observer[(Element, Element)]
}

sealed trait HookPairOption extends Hook {
  def observer: Observer[(Option[Element], Option[Element])]
}

final case class InsertHook(observer: Observer[Element]) extends HookSingle
final case class PrePatchHook(observer: Observer[(Option[Element], Option[Element])]) extends HookPairOption
final case class UpdateHook(observer: Observer[(Element, Element)]) extends HookPair
final case class PostPatchHook(observer: Observer[(Element, Element)]) extends HookPair
final case class DestroyHook(observer: Observer[Element]) extends HookSingle


final case class AttributeStreamReceiver(attribute: String, attributeStream: Observable[Attribute]) extends VDomModifier_

case object EmptyVDomModifier extends VDomModifier_

sealed trait ChildVNode extends VDomModifier_

final case class ChildStreamReceiver(childStream: Observable[VNode]) extends ChildVNode
final case class ChildrenStreamReceiver(childrenStream: Observable[Seq[VNode]]) extends ChildVNode

sealed trait VNode_ extends ChildVNode {
  def apply(args: VDomModifier*): VNode
  // TODO: rename asProxy to asSnabbdom?
  def asProxy: VNodeProxy
}

//TODO: extends AnyVal
private[outwatch] final case class StringNode(string: String) extends VNode_ {
  override def apply(args: VDomModifier*): VNode = ???
  override val asProxy: VNodeProxy = VNodeProxy.fromString(string)
}

// TODO: instead of Seq[VDomModifier] use Vector or JSArray?
// Fast concatenation and lastOption operations are important
// Needs to be benchmarked in the Browser
private[outwatch] final case class VTree(nodeType: String,
                       modifiers: Seq[VDomModifier]) extends VNode_ {

  override def apply(args: VDomModifier*) = IO.pure(VTree(nodeType, modifiers ++ args))

  override def asProxy = {
    val modifiers_ = modifiers.map(_.unsafeRunSync())
    val (children, attributeObject) = DomUtils.extractChildrenAndDataObject(modifiers_)
    //TODO: use .sequence instead of unsafeRunSync?
    // import cats.instances.list._
    // import cats.syntax.traverse._
    // for { childProxies <- children.map(_.value).sequence }
    // yield h(nodeType, attributeObject, childProxies.map(_.apsProxy)(breakOut))
    val childProxies: js.Array[VNodeProxy] = children.map(_.asProxy)(breakOut)
    hFunction(nodeType, attributeObject, childProxies)
  }

  // ensure a key is present in the VTree modifiers
  // used to ensure efficient Snabbdom patch operation in the presence of children streams
  private[outwatch] def ensureKey: VTree = {
    val hasKey = modifiers.exists(m => m.unsafeRunSync().isInstanceOf[Key])
    val newModifiers = if (hasKey) modifiers else IO.pure(Key(this.hashCode)) +: modifiers
    copy(modifiers = newModifiers)
  }
}





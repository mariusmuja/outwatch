package outwatch.dom

import cats.effect.IO
import monix.execution.Ack
import monix.reactive.Observer
import org.scalajs.dom._
import outwatch.dom.helpers.SeparatedModifiers
import snabbdom.{DataObject, VNodeProxy}

import scala.concurrent.Future


/*
Modifier
  Property
    Attribute
      Attr
      AccumAttr
      Prop
      Style
      EmptyAttribute
    Hook
      InsertHook
      PrePatchHook
      UpdateHook
      PostPatchHook
      DestroyHook
    Key
  ChildVNode
    StaticVNode
      StringVNode
      VTree
    ChildStreamReceiver
    ChildrenStreamReceiver
  Emitter
  AttributeStreamReceiver
  CompositeModifier
  StringModifier
  EmptyModifier
 */

private[outwatch] sealed trait Modifier extends Any

// Modifiers

private[outwatch] sealed trait Property extends Modifier

private[outwatch] sealed trait ChildVNode extends Any with Modifier

final case class Emitter(eventType: String, trigger: Event => Future[Ack]) extends Modifier

private[outwatch] final case class AttributeStreamReceiver(attribute: String, attributeStream: Observable[Attribute]) extends Modifier

private[outwatch] final case class CompositeModifier(modifiers: Seq[VDomModifier]) extends Modifier

private[outwatch] final case class StringModifier(string: String) extends Modifier

case object EmptyModifier extends Modifier

// Properties

sealed trait Attribute extends Property {
  val title: String
}
object Attribute {
  def apply(title: String, value: Attr.Value) = Attr(title, value)
}

sealed trait Hook[T] extends Property {
  def observer: Observer[T]
}

private[outwatch] final case class Key(value: Key.Value) extends Property
object Key {
  type Value = DataObject.KeyValue
}

// Attributes

final case class Attr(title: String, value: Attr.Value) extends Attribute
object Attr {
  type Value = DataObject.AttrValue
}

/**
  * Attribute that accumulates the previous value in the same VNode with it's value
  */
final case class AccumAttr(title: String, value: Attr.Value, accum: (Attr.Value, Attr.Value)=> Attr.Value) extends Attribute

final case class ClassToggle(title: String, toggle: Boolean) extends Attribute

final case class Prop(title: String, value: Prop.Value) extends Attribute
object Prop {
  type Value = DataObject.PropValue
}

final case class Style(title: String, value: String) extends Attribute

case object EmptyAttribute extends Attribute {
  val title: String = ""
}
// Hooks

private[outwatch] final case class InsertHook(observer: Observer[Element]) extends Hook[Element]
private[outwatch] final case class PrePatchHook(observer: Observer[(Option[Element], Option[Element])])
  extends Hook[(Option[Element], Option[Element])]
private[outwatch] final case class UpdateHook(observer: Observer[(Element, Element)]) extends Hook[(Element, Element)]
private[outwatch] final case class PostPatchHook(observer: Observer[(Element, Element)]) extends Hook[(Element, Element)]
private[outwatch] final case class DestroyHook(observer: Observer[Element]) extends Hook[Element]

// Child Nodes
private[outwatch] sealed trait StaticVNode extends Any with ChildVNode {
  def toSnabbdom: VNodeProxy
}

private[outwatch] final case class ChildStreamReceiver(childStream: Observable[IO[StaticVNode]]) extends ChildVNode

private[outwatch] final case class ChildrenStreamReceiver(childrenStream: Observable[Seq[IO[StaticVNode]]]) extends ChildVNode

// Static Nodes
private[outwatch] final case class StringVNode(string: String) extends AnyVal with StaticVNode {
  override def toSnabbdom: VNodeProxy = VNodeProxy.fromString(string)
}

// TODO: instead of Seq[VDomModifier] use Vector or JSArray?
// Fast concatenation and lastOption operations are important
// Needs to be benchmarked in the Browser
private[outwatch] final case class VTree(nodeType: String,
                       modifiers: Seq[VDomModifier]) extends StaticVNode {

  def apply(args: VDomModifier*): VNode = IO.pure(VTree(nodeType, modifiers ++ args))

  override def toSnabbdom: VNodeProxy = {
    //TODO: use .sequence instead of unsafeRunSync?
    // import cats.instances.list._
    // import cats.syntax.traverse._
    val separatedModifiers = SeparatedModifiers.separate(modifiers.map(_.unsafeRunSync()))
    separatedModifiers.toSnabbdom(nodeType)
  }
}





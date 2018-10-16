package outwatch.dom

import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import org.scalajs.dom._
import outwatch.dom.helpers.VNodeState
import snabbdom.{DataObject, VNodeProxy}

import scala.concurrent.Future

/*
Modifier
  ModifierStream
  StreamableModifier
    Emitter
    CompositeModifier
    Attribute
      Attr
        BasicAttr
        AccumAttr
      Prop
      Style
        BasicStyle
        AccumStyle
        DelayedStyle
        RemoveStyle
        DestroyStyle
    Hook
      InsertHook
      PrePatchHook
      UpdateHook
      PostPatchHook
      DestroyHook
    StaticVNode
      StringVNode
      VTree
    StringModifier
    Key
    EmptyModifier
 */


private[outwatch] sealed trait Modifier extends Any

private[outwatch] final case class ModifierStream(stream: Observable[VDomModifier]) extends Modifier

// optimization for streaming VNodes
private[outwatch] final case class VNodeStream(stream: Observable[VNode]) extends Modifier

private[outwatch] sealed trait StreamableModifier extends Modifier

private[outwatch] final case class CompositeModifier(modifiers: Seq[Modifier]) extends StreamableModifier


// Modifiers

private[outwatch] final case class Emitter(eventType: String, trigger: Event => Future[Ack]) extends StreamableModifier

private[outwatch] sealed trait Attribute extends StreamableModifier {
  val title: String
}

private[outwatch] sealed trait Hook[T] extends StreamableModifier {
  def observer: Observer[T]
}

private[outwatch] sealed trait StaticVNode extends StreamableModifier {
  def toSnabbdom(implicit s: Scheduler): VNodeProxy
}

private[outwatch] final case class Key(value: Key.Value) extends StreamableModifier
object Key {
  type Value = DataObject.KeyValue
}

private[outwatch] case object EmptyModifier extends StreamableModifier

// Attributes
private[outwatch] sealed trait Attr extends Attribute {
  val value: Attr.Value
}
object Attr {
  type Value = DataObject.AttrValue
}

private[outwatch] final case class BasicAttr(title: String, value: Attr.Value) extends Attr

/**
  * Attribute that accumulates the previous value in the same VNode with it's value
  */
private[outwatch] final case class AccumAttr(title: String, value: Attr.Value, accum: (Attr.Value, Attr.Value)=> Attr.Value) extends Attr

private[outwatch] final case class Prop(title: String, value: Prop.Value) extends Attribute
object Prop {
  type Value = DataObject.PropValue
}

private[outwatch] sealed trait Style extends Attribute {
  val value: String
}
object Style {
  type Value = DataObject.StyleValue
}


private[outwatch] final case class BasicStyle(title: String, value: String) extends Style
private[outwatch] final case class AccumStyle(title: String, value: String, accum: (String, String) => String) extends Style

private[outwatch] final case class DelayedStyle(title: String, value: String) extends Style
private[outwatch] final case class RemoveStyle(title: String, value: String) extends Style
private[outwatch] final case class DestroyStyle(title: String, value: String) extends Style

// Hooks
private[outwatch] final case class InsertHook(observer: Observer[Element]) extends Hook[Element]
private[outwatch] final case class PrePatchHook(observer: Observer[(Option[Element], Option[Element])]) extends Hook[(Option[Element], Option[Element])]
private[outwatch] final case class UpdateHook(observer: Observer[(Element, Element)]) extends Hook[(Element, Element)]
private[outwatch] final case class PostPatchHook(observer: Observer[(Element, Element)]) extends Hook[(Element, Element)]
private[outwatch] final case class DestroyHook(observer: Observer[Element]) extends Hook[Element]


// Static Nodes
private[outwatch] final case class StringVNode(string: String) extends StaticVNode {
  override def toSnabbdom(implicit s: Scheduler): VNodeProxy = VNodeProxy.fromString(string)
}

// TODO: instead of Seq[VDomModifier] use Vector or JSArray?
// Fast concatenation and lastOption operations are important
// Needs to be benchmarked in the Browser
private[outwatch] final case class VTree(nodeType: String, modifiers: Seq[Modifier]) extends StaticVNode {

  def apply(args: VDomModifier*): VNode = args.sequence.map(args => copy(modifiers = modifiers ++ args))

  override def toSnabbdom(implicit s: Scheduler): VNodeProxy = {
    VNodeState.from(modifiers).toSnabbdom(nodeType)
  }
}







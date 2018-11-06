package outwatch.dom

import monix.execution.Scheduler
import monix.reactive.Observer
import org.scalajs.dom._
import outwatch.dom.helpers.VNodeState
import snabbdom.{DataObject, VNodeProxy}


private[outwatch] sealed trait Modifier extends Any

private[outwatch] final case class CompositeModifier(modifiers: Seq[Modifier]) extends Modifier

private[outwatch] sealed trait FlatModifier extends Modifier

private[outwatch] final case class ModifierStream(stream: Observable[Modifier]) extends FlatModifier

private[outwatch] sealed trait SimpleModifier extends FlatModifier


private[outwatch] final case class SimpleCompositeModifier(modifiers: Seq[SimpleModifier]) extends SimpleModifier

// Modifiers

private[outwatch] final case class Emitter(eventType: String, trigger: Event => Unit) extends SimpleModifier

private[outwatch] sealed trait Attribute extends SimpleModifier {
  val title: String
}

private[outwatch] sealed trait Hook[T] extends SimpleModifier {
  def observer: Observer[T]
}

private[outwatch] sealed trait LifecycleHook extends SimpleModifier {
  def fn: (VNodeProxy, Scheduler) => Unit
}

private[outwatch] sealed trait StaticVNode extends SimpleModifier {
  def toSnabbdom(implicit s: Scheduler): VNodeProxy
}

private[outwatch] final case class Key(value: Key.Value) extends SimpleModifier
object Key {
  type Value = DataObject.KeyValue
}

private[outwatch] case object EmptyModifier extends SimpleModifier

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

private[outwatch] sealed trait Style extends Attribute
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

private[outwatch] final case class InsertProxyHook(fn: (VNodeProxy, Scheduler) => Unit) extends LifecycleHook
private[outwatch] final case class DestroyProxyHook(fn: (VNodeProxy, Scheduler) => Unit) extends LifecycleHook

// Static Nodes
private[outwatch] final case class StringVNode(string: String) extends StaticVNode {
  override def toSnabbdom(implicit s: Scheduler): VNodeProxy = VNodeProxy.fromString(string)
}


private class Memoized[T] {
  private var value: Option[T] = None

  def getOrUpdate(v: => T): T = this.synchronized {
    value.getOrElse{
      val v2 = v
      value = Some(v2)
      v2
    }
  }
}


private[outwatch] final case class VTree(nodeType: String, modifiers: Array[Modifier] = Array.empty) extends StaticVNode {

  def apply(args: VDomModifier*): VNode = {
    args.sequence.map(args => copy(modifiers = modifiers ++ args))
  }

  private val proxy = new Memoized[VNodeProxy]
  override def toSnabbdom(implicit s: Scheduler): VNodeProxy = {
    proxy.getOrUpdate(
      VNodeState.from(modifiers).toSnabbdom(nodeType)
    )
  }
}







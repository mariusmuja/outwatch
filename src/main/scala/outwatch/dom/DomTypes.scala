package outwatch.dom

import com.raquo.domtypes.generic.builders._
import com.raquo.domtypes.generic.keys
import com.raquo.domtypes.generic.codecs._
import com.raquo.domtypes.generic.defs.attrs._
import com.raquo.domtypes.generic.defs.reflectedAttrs._
import com.raquo.domtypes.generic.defs.props._
import com.raquo.domtypes.generic.defs.styles._
import com.raquo.domtypes.generic.defs.sameRefTags._
import com.raquo.domtypes.jsdom.defs.eventProps._
import cats.effect.IO
import org.scalajs.dom
import helpers._

trait DomTypes
  extends DomTags
  with DomAttrs
  with DomReflectedAttrs
  with DomProps
  with DomEvents
  with DomStyles

private[outwatch] object typehelper {
  type GenericVNode[T] = VNode_
}
import typehelper._

trait VNodeBuilder extends TagBuilder[GenericVNode, VNode_] {
  // we can ignore information about void tags here, because snabbdom handles this automatically for us based on the tagname.
  override def tag[Ref <: VNode_](tagName: String, void: Boolean): VNode_ =
    VTree(tagName, Seq.empty)
}

trait DomTags
  extends GroupingTags[GenericVNode, VNode_]
  with TextTags[GenericVNode, VNode_]
  with FormTags[GenericVNode, VNode_]
  with SectionTags[GenericVNode, VNode_]
  with TableTags[GenericVNode, VNode_]
  with VNodeBuilder
object DomTags extends DomTags

trait DomTagsExtra
  extends DocumentTags[GenericVNode, VNode_]
  with EmbedTags[GenericVNode, VNode_]
  with MiscTags[GenericVNode, VNode_]
  with VNodeBuilder
object DomTagsExtra extends DomTagsExtra

object CodecBuilder {
  type Attribute[T, _] = AttributeBuilder[T]
  type Property[T, _] = PropertyBuilder[T]

  def encodeProperty[V](codec: Codec[V, _]): V => String =
    // codec.encode may encode to null values
    (codec.encode _) andThen (v => if (v != null) v.toString else null)

  def encodeAttribute[V](codec: Codec[V, String]): V => Attribute.Value = codec match {
    //The BooleanAsAttrPresenceCodec does not play well with snabbdom. it
    //encodes true as "" and false as null, whereas snabbdom needs true/false
    //of type boolean (not string) for toggling the presence of the attribute.
    case _: BooleanAsAttrPresenceCodec.type => identity
    case _ => codec.encode _
  }
}

trait DomAttrs
  extends Attrs[AttributeBuilder]
  with AttrBuilder[AttributeBuilder] {

  override protected def attr[V](key: String, codec: Codec[V, String]): AttributeBuilder[V] =
    new AttributeBuilder(key, codec.encode _)
}
object DomAttrs extends DomAttrs

trait DomReflectedAttrs
  extends ReflectedAttrs[CodecBuilder.Attribute]
  with ReflectedAttrBuilder[CodecBuilder.Attribute] {

  override protected def reflectedAttr[V, DomPropV](
    attrKey: String,
    propKey: String,
    attrCodec: Codec[V, String],
    propCodec: Codec[V, DomPropV]
  ): AttributeBuilder[V] =
    new AttributeBuilder(attrKey, CodecBuilder.encodeAttribute(attrCodec))
    //or: new PropertyBuilder(propKey, CodecBuilder.encodeProperty(propCodec))
}
object DomReflectedAttrs extends DomReflectedAttrs

trait DomProps
  extends Props[CodecBuilder.Property]
  with PropBuilder[CodecBuilder.Property] {

  override protected def prop[V, DomV](key: String, codec: Codec[V, DomV]): PropertyBuilder[V] =
    new PropertyBuilder(key, CodecBuilder.encodeProperty(codec))
}
object DomProps extends DomProps

trait DomEvents
  extends MouseEventProps[SimpleEmitterBuilder]
  with FormEventProps[SimpleEmitterBuilder]
  with KeyboardEventProps[SimpleEmitterBuilder]
  with ClipboardEventProps[SimpleEmitterBuilder]
  with ErrorEventProps[SimpleEmitterBuilder]
  with EventPropBuilder[SimpleEmitterBuilder, dom.Event] {

  override def eventProp[V <: dom.Event](key: String): SimpleEmitterBuilder[V] =
    EmitterBuilder[V](key)
}
object DomEvents extends DomEvents

trait SimpleStyleBuilder extends StyleBuilders[IO[Style]] {

  implicit def StyleIsBuilder[T](style: keys.Style[T]): StyleBuilder[T] = new StyleBuilder[T](style.cssName)

  override def buildDoubleStyleSetter(style: keys.Style[Double], value: Double): IO[Style] = style := value
  override def buildIntStyleSetter(style: keys.Style[Int],value: Int): IO[Style] = style := value
  override def buildStringStyleSetter(style: keys.Style[_],value: String): IO[Style] = new StyleBuilder[Any](style.cssName) := value
}

trait DomStyles
  extends Styles[IO[Style]]
  with SimpleStyleBuilder
object DomStyles extends DomStyles

trait DomStylesExtra
  extends Styles2[IO[Style]]
  with SimpleStyleBuilder
object DomStylesExtra extends DomStylesExtra

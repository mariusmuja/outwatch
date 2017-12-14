package outwatch.dom

import com.raquo.domtypes.generic.builders
import com.raquo.domtypes.generic.keys
import com.raquo.domtypes.generic.codecs._
import com.raquo.domtypes.generic.defs.attrs
import com.raquo.domtypes.generic.defs.reflectedAttrs
import com.raquo.domtypes.generic.defs.props
import com.raquo.domtypes.generic.defs.styles
import com.raquo.domtypes.generic.defs.sameRefTags._
import com.raquo.domtypes.jsdom.defs.eventProps._
import cats.effect.IO
import org.scalajs.dom
import helpers._
import monix.execution.{Ack, Cancelable}
import monix.reactive.OverflowStrategy.Unbounded

import scala.scalajs.js

private[outwatch] object DomTypes {
  type AttributeBuilder[T, _] = helpers.AttributeBuilder[T, Attr]
  type PropertyBuilder[T, _] = helpers.PropBuilder[T]
}

private[outwatch] object CodecBuilder {
  def encodeAttribute[V](codec: Codec[V, String]): V => Attr.Value = codec match {
    //The BooleanAsAttrPresenceCodec does not play well with snabbdom. it
    //encodes true as "" and false as null, whereas snabbdom needs true/false
    //of type boolean (not string) for toggling the presence of the attribute.
    case _: BooleanAsAttrPresenceCodec.type => identity
    case _ => codec.encode
  }
}

// Tags

private[outwatch] trait TagBuilder extends builders.TagBuilder[TagBuilder.Tag, VTree] {
  // we can ignore information about void tags here, because snabbdom handles this automatically for us based on the tagname.
  protected override def tag[Ref <: VTree](tagName: String, void: Boolean): VTree = VTree(tagName, Seq.empty)
}
private[outwatch] object TagBuilder {
  type Tag[T] = VTree
}

trait Tags
  extends EmbedTags[TagBuilder.Tag, VTree]
          with GroupingTags[TagBuilder.Tag, VTree]
          with TextTags[TagBuilder.Tag, VTree]
          with FormTags[TagBuilder.Tag, VTree]
          with SectionTags[TagBuilder.Tag, VTree]
          with TableTags[TagBuilder.Tag, VTree]
          with TagsCompat
          with TagBuilder
          with TagHelpers
object Tags extends Tags {
  object all extends Tags with TagsExtra
}

trait TagsExtra
  extends DocumentTags[TagBuilder.Tag, VTree]
          with MiscTags[TagBuilder.Tag, VTree]
          with TagBuilder
object TagsExtra extends TagsExtra

// all Attributes

trait Attributes
  extends Attrs
  with ReflectedAttrs
  with Props
  with Events
  with Styles
  with AttributesCompat
  with OutwatchAttributes
object Attributes extends Attributes

// Attrs

trait Attrs
  extends attrs.Attrs[AttrBuilder]
  with builders.AttrBuilder[AttrBuilder] {

  override protected def attr[V](key: String, codec: Codec[V, String]): AttrBuilder[V] =
    new AttrBuilder(key, CodecBuilder.encodeAttribute(codec))
}
object Attrs extends Attrs

// Reflected attrs

trait ReflectedAttrs
  extends reflectedAttrs.ReflectedAttrs[DomTypes.AttributeBuilder]
  with builders.ReflectedAttrBuilder[DomTypes.AttributeBuilder] {

  override lazy val className = new AccumAttrBuilder[String]("class", _.toString, _ + " " + _)

  override protected def reflectedAttr[V, DomPropV](
    attrKey: String,
    propKey: String,
    attrCodec: Codec[V, String],
    propCodec: Codec[V, DomPropV]
  ) = new AttrBuilder(attrKey, CodecBuilder.encodeAttribute(attrCodec))
    //or: new PropertyBuilder(propKey, propCodec.encode)
}
object ReflectedAttrs extends ReflectedAttrs

// Props

trait Props
  extends props.Props[DomTypes.PropertyBuilder]
  with builders.PropBuilder[DomTypes.PropertyBuilder] {

  override protected def prop[V, DomV](key: String, codec: Codec[V, DomV]): PropBuilder[V] =
    new PropBuilder(key, codec.encode)
}
object Props extends Props

// Events

trait Events
  extends HTMLElementEventProps[SimpleEmitterBuilder]
  with builders.EventPropBuilder[SimpleEmitterBuilder, dom.Event] {

  override def eventProp[V <: dom.Event](key: String): SimpleEmitterBuilder[V] =  EmitterBuilder[V](key)
}
object Events extends Events


// Window / Document events

private[outwatch] abstract class ObservableEventPropBuilder(target: dom.EventTarget)
  extends builders.EventPropBuilder[Observable, dom.Event] {
  override def eventProp[V <: dom.Event](key: String): Observable[V] = Observable.create(Unbounded) { obs =>
    val eventHandler: js.Function1[V, Ack] = obs.onNext _
    target.addEventListener(key, eventHandler)
    Cancelable(() => target.removeEventListener(key, eventHandler))
  }
}

object WindowEvents
  extends ObservableEventPropBuilder(dom.window)
  with WindowEventProps[Observable]

object DocumentEvents
  extends ObservableEventPropBuilder(dom.document)
  with DocumentEventProps[Observable]

// Styles

private[outwatch] trait SimpleStyleBuilder extends builders.StyleBuilders[IO[Style]] {
  override protected def buildDoubleStyleSetter(style: keys.Style[Double], value: Double): IO[Style] = style := value
  override protected def buildIntStyleSetter(style: keys.Style[Int],value: Int): IO[Style] = style := value
  override protected def buildStringStyleSetter(style: keys.Style[_],value: String): IO[Style] = new BasicStyleBuilder[String](style.cssName) := value
}

trait Styles
  extends styles.Styles[IO[Style]]
  with SimpleStyleBuilder
object Styles extends Styles {
  object all extends Styles with StylesExtra
}

trait StylesExtra
  extends styles.Styles2[IO[Style]]
  with SimpleStyleBuilder
object StylesExtra extends StylesExtra

// everything

object all extends Attributes with Tags with HandlerFactories
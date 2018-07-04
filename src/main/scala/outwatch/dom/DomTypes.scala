package outwatch.dom

import cats.effect.IO
import com.raquo.domtypes.generic.defs.sameRefTags._
import com.raquo.domtypes.generic.defs.{attrs, props, reflectedAttrs, styles, tags}
import com.raquo.domtypes.generic.{builders, codecs, keys}
import com.raquo.domtypes.jsdom.defs.eventProps
import monix.execution.{Ack, Cancelable}
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom
import outwatch.dom.helpers._

import scala.scalajs.js

private[outwatch] object BuilderTypes {
  type Attribute[T, _] = helpers.AttributeBuilder[T, Attr]
  type Property[T, _] = helpers.PropBuilder[T]
  type EventEmitter[E <: dom.Event] = SimpleEmitterBuilder[E, Emitter]

  type Tag[T] = VTree

  type SameRefSvgTags[T[_ <: N], N] = tags.SvgTags[T, N,
    N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N,
    N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N,
    N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N]
}

private[outwatch] object CodecBuilder {
  def encodeAttribute[V](codec: codecs.Codec[V, String]): V => Attr.Value = codec match {
    //The BooleanAsAttrPresenceCodec does not play well with snabbdom. it
    //encodes true as "" and false as null, whereas snabbdom needs true/false
    //of type boolean (not string) for toggling the presence of the attribute.
    case _: codecs.BooleanAsAttrPresenceCodec.type => identity
    case _ => codec.encode
  }
}

// Tags

private[outwatch] trait HtmlTagBuilder extends builders.HtmlTagBuilder[BuilderTypes.Tag, VTree] {
  // we can ignore information about void tags here, because snabbdom handles this automatically for us based on the tagname.
  protected override def htmlTag[Ref <: VTree](tagName: String, void: Boolean): VTree = VTree(tagName, Seq.empty)
}


trait Tags
  extends EmbedTags[BuilderTypes.Tag, VTree]
  with GroupingTags[BuilderTypes.Tag, VTree]
  with TextTags[BuilderTypes.Tag, VTree]
  with FormTags[BuilderTypes.Tag, VTree]
  with SectionTags[BuilderTypes.Tag, VTree]
  with TableTags[BuilderTypes.Tag, VTree]
  with HtmlTagBuilder
  with TagHelpers
  with TagsCompat

@deprecated("Use dsl.tags instead", "0.11.0")
object Tags extends Tags

trait TagsExtra
  extends DocumentTags[BuilderTypes.Tag, VTree]
  with MiscTags[BuilderTypes.Tag, VTree]
  with HtmlTagBuilder

trait TagsSvg
  extends BuilderTypes.SameRefSvgTags[BuilderTypes.Tag, VTree]
  with builders.SvgTagBuilder[BuilderTypes.Tag, VTree] {

  protected override def svgTag[Ref <: VTree](tagName: String, void: Boolean): VTree = VTree(tagName, Seq.empty)
}


// all Attributes

trait Attributes
  extends Attrs
  with ReflectedAttrs
  with Props
  with Events
  with AttributeHelpers
  with OutwatchAttributes
  with AttributesCompat
  with OutWatchChildAttributesCompat

@deprecated("Use dsl.attributes instead", "0.11.0")
object Attributes extends Attributes

// Attrs

trait Attrs
  extends attrs.Attrs[BasicAttrBuilder]
  with builders.HtmlAttrBuilder[BasicAttrBuilder] {

  override protected def htmlAttr[V](key: String, codec: codecs.Codec[V, String]): BasicAttrBuilder[V] =
    new BasicAttrBuilder(key, CodecBuilder.encodeAttribute(codec))
}

// Svg attributes
trait AttrsSvg
  extends attrs.SvgAttrs[BasicAttrBuilder]
  with builders.SvgAttrBuilder[BasicAttrBuilder] {

  override protected def svgAttr[V](key: String, codec: codecs.Codec[V, String]): BasicAttrBuilder[V] =
    new BasicAttrBuilder(key, CodecBuilder.encodeAttribute(codec))
}

// Reflected attrs

trait ReflectedAttrs
  extends reflectedAttrs.ReflectedAttrs[BuilderTypes.Attribute]
  with builders.ReflectedAttrBuilder[BuilderTypes.Attribute] {

  // super.className.accum(" ") would have been nicer, but we can't do super.className on a lazy val
  override lazy val className = new AccumAttrBuilder[String]("class",
    stringReflectedAttr(attrKey = "class", propKey = "className"),
    _ + " " + _
  )

  override protected def reflectedAttr[V, DomPropV](
    attrKey: String,
    propKey: String,
    attrCodec: codecs.Codec[V, String],
    propCodec: codecs.Codec[V, DomPropV]
  ) = new BasicAttrBuilder(attrKey, CodecBuilder.encodeAttribute(attrCodec))
    //or: new PropertyBuilder(propKey, propCodec.encode)
}

// Props
trait Props
  extends props.Props[BuilderTypes.Property]
  with builders.PropBuilder[BuilderTypes.Property] {

  override protected def prop[V, DomV](key: String, codec: codecs.Codec[V, DomV]): PropBuilder[V] =
    new PropBuilder(key, codec.encode)
}


// Events
trait Events
  extends eventProps.HTMLElementEventProps[BuilderTypes.EventEmitter]
  with builders.EventPropBuilder[BuilderTypes.EventEmitter, dom.Event] {

  override def eventProp[V <: dom.Event](key: String): BuilderTypes.EventEmitter[V] =  EmitterBuilder[V](key)
}


// Window / Document events

private[outwatch] abstract class ObservableEventPropBuilder(target: dom.EventTarget)
  extends builders.EventPropBuilder[Observable, dom.Event] {
  override def eventProp[V <: dom.Event](key: String): Observable[V] = Observable.create(Unbounded) { obs =>
    val eventHandler: js.Function1[V, Ack] = obs.onNext _
    target.addEventListener(key, eventHandler)
    Cancelable(() => target.removeEventListener(key, eventHandler))
  }
}

abstract class WindowEvents
  extends ObservableEventPropBuilder(dom.window)
  with eventProps.WindowEventProps[Observable]

abstract class DocumentEvents
  extends ObservableEventPropBuilder(dom.document)
  with eventProps.DocumentEventProps[Observable]

// Styles

private[outwatch] trait SimpleStyleBuilder extends builders.StyleBuilders[IO[Style]] {
  override protected def buildDoubleStyleSetter(style: keys.Style[Double], value: Double): IO[Style] = style := value
  override protected def buildIntStyleSetter(style: keys.Style[Int], value: Int): IO[Style] = style := value
  override protected def buildStringStyleSetter(style: keys.Style[_], value: String): IO[Style] = new BasicStyleBuilder[Any](style.cssName) := value
}

trait Styles
  extends styles.Styles[IO[Style]]
  with SimpleStyleBuilder

trait StylesExtra
  extends styles.Styles2[IO[Style]]
  with SimpleStyleBuilder

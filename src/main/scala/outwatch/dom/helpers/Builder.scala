package outwatch.dom.helpers

import cats.effect.IO
import outwatch.StaticVNodeRender

import scala.language.dynamics
import outwatch.dom._

import scala.language.implicitConversions

trait ValueBuilder[T, SELF <: Attribute] extends Any {
  protected def attributeName: String
  protected def assign(value: T): SELF

  def :=(value: T): IO[SELF] = IO.pure(assign(value))
  def :=?(value: Option[T]): Option[VDomModifier] = value.map(:=)
  def <--(valueStream: Observable[T]): IO[AttributeStreamReceiver] = {
    IO.pure(AttributeStreamReceiver(attributeName, valueStream.map(assign)))
  }
}

final class AttributeBuilder[T](val attributeName: String, encode: T => Attr.Value) extends ValueBuilder[T, Attr] {
  @inline protected def assign(value: T) = Attr(attributeName, encode(value))

  def accum(reduce: (Attr.Value, Attr.Value) => Attr.Value): AccumAttributeBuilder[T] =
    new AccumAttributeBuilder(attributeName, encode, reduce)
}

object AttributeBuilder {
  implicit def toAttribute(builder: AttributeBuilder[Boolean]): IO[Attribute] = IO.pure(builder assign true)
}

final class AccumAttributeBuilder[T](
  val attributeName: String,
  encode: T => Attr.Value,
  reduce: (Attr.Value, Attr.Value) => Attr.Value
) extends ValueBuilder[T, AccumAttr] {
  @inline protected def assign(value: T) = AccumAttr(attributeName, encode(value), reduce)
}


final class ClassToggleBuilder(
  val attributeName: String
) extends ValueBuilder[(String, Boolean), ClassToggle] {
  @inline protected def assign(value: (String, Boolean)) = ClassToggle(value._1, value._2)
}


final class PropertyBuilder[T](val attributeName: String, encode: T => Prop.Value) extends ValueBuilder[T, Prop] {
  @inline protected def assign(value: T) = Prop(attributeName, encode(value))
}

object PropertyBuilder {
  implicit def toProperty(builder: PropertyBuilder[Boolean]): IO[Property] = IO.pure(builder assign true)
}

final class StyleBuilder[T](val attributeName: String) extends AnyVal with ValueBuilder[T, Style] {
  @inline protected def assign(value: T) = Style(attributeName, value.toString)
}

final class DynamicAttributeBuilder[T](parts: List[String]) extends Dynamic with ValueBuilder[T, Attr] {
  lazy val attributeName: String = parts.reverse.mkString("-")

  def selectDynamic(s: String) = new DynamicAttributeBuilder[T](s :: parts)

  @inline protected def assign(value: T) = Attr(attributeName, value.toString)
}

object KeyBuilder {
  def :=(key: Key.Value): IO[Key] = IO.pure(Key(key))
}

object ChildStreamReceiverBuilder {
  def <--[T](valueStream: Observable[T])(implicit r: StaticVNodeRender[T]): IO[ChildStreamReceiver] = {
    IO.pure(ChildStreamReceiver(valueStream.map(r.render)))
  }
}

object ChildrenStreamReceiverBuilder {
  def <--(childrenStream: Observable[Seq[VNode]]): IO[ChildrenStreamReceiver] = {
    IO.pure(ChildrenStreamReceiver(childrenStream))
  }
}

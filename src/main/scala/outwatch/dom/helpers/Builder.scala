package outwatch.dom.helpers

import cats.effect.IO

import scala.language.dynamics
import outwatch.dom._
import rxscalajs.Observable

import scala.language.implicitConversions
import outwatch.dom.StringNode

trait ValueBuilder[T, SELF <: Attribute] extends Any {
  protected def attributeName: String
  protected def assign(value: T): SELF

  def :=(value: T): IO[SELF] = IO.pure(assign(value))
  def :=?(value: Option[T]): Option[VDomModifier] = value.map(:=)
  def <--(valueStream: Observable[T]): IO[AttributeStreamReceiver] = {
    IO.pure(AttributeStreamReceiver(attributeName, valueStream.map(assign)))
  }
}

final class AttributeBuilder[T](val attributeName: String, encode: T => Attribute.Value = (t: T) => t.toString) extends ValueBuilder[T, Attribute] {
  @inline protected def assign(value: T) = Attribute(attributeName, encode(value))
}
object AttributeBuilder {
  implicit def toAttribute(builder: AttributeBuilder[Boolean]): IO[Attribute] = IO.pure(builder assign true)
}

final class PropertyBuilder[T](val attributeName: String, encode: T => String = (t: T) => t.toString) extends ValueBuilder[T, Prop] {
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

  @inline protected def assign(value: T) = Attribute(attributeName, value.toString)
}

object KeyBuilder {
  def :=(key: String) = IO.pure(Key(key))
}

object ChildStreamReceiverBuilder {
  def <--[T <: Any](valueStream: Observable[T]): IO[ChildStreamReceiver] = {
    IO.pure(ChildStreamReceiver(valueStream.map(anyToVNode)))
  }

  private val anyToVNode: Any => VNode = {
    case vn: IO[_] => vn.asInstanceOf[VNode]
    case any => IO.pure(StringNode(any.toString))
  }
}

object ChildrenStreamReceiverBuilder {
  def <--(childrenStream: Observable[Seq[VNode]]) = {
    IO.pure(ChildrenStreamReceiver(childrenStream))
  }
}

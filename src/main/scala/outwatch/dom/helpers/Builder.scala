package outwatch.dom.helpers

import cats.effect.IO
import outwatch.StaticVNodeRender

import scala.language.dynamics
import outwatch.dom._

import scala.language.implicitConversions

trait AttributeBuilder[-T, +A <: Attribute] extends Any {
  protected def name: String
  private[outwatch] def assign(value: T): A

  def :=(value: T): IO[A] = IO.pure(assign(value))
  def :=?(value: Option[T]): Option[VDomModifier] = value.map(:=)
  def <--(valueStream: Observable[T]): IO[AttributeStreamReceiver] = {
    IO.pure(AttributeStreamReceiver(name, valueStream.map(assign)))
  }
}
object AttributeBuilder {
  implicit def toAttribute(builder: AttributeBuilder[Boolean, Attr]): IO[Attribute] = builder := true
  implicit def toProperty(builder: AttributeBuilder[Boolean, Prop]): IO[Property] = builder := true
}

// Attr

final class AttrBuilder[T](val name: String, encode: T => Attr.Value) extends AttributeBuilder[T, Attr] {
  @inline private[outwatch] def assign(value: T) = BasicAttr(name, encode(value))

  def accum(s: String): AccumAttrBuilder[T] = accum(_ + s + _)
  def accum(reducer: (Attr.Value, Attr.Value) => Attr.Value) = new AccumAttrBuilder[T](name, encode, reducer)
}


final class AccumAttrBuilder[T](
  val name: String,
  encode: T => Attr.Value,
  reduce: (Attr.Value, Attr.Value) => Attr.Value
) extends AttributeBuilder[T, Attr] {
  @inline private[outwatch] def assign(value: T) = AccumAttr(name, encode(value), reduce)
}

final class DynamicAttributeBuilder[T](parts: List[String]) extends Dynamic with AttributeBuilder[T, Attr] {
  lazy val name: String = parts.reverse.mkString("-")

  def selectDynamic(s: String) = new DynamicAttributeBuilder[T](s :: parts)

  @inline private[outwatch] def assign(value: T) = BasicAttr(name, value.toString)
}

// Props

final class PropBuilder[T](val name: String, encode: T => Prop.Value) extends AttributeBuilder[T, Prop] {
  @inline private[outwatch] def assign(value: T) = Prop(name, encode(value))
}

// Styles
final class BasicStyleBuilder[T](val name: String) extends AnyVal with AttributeBuilder[T, BasicStyle] {
  @inline private[outwatch] def assign(value: T) = BasicStyle(name, value.toString)

  def accum: AccumStyleBuilder[T, BasicStyle] = accum(",")
  def accum(s: String): AccumStyleBuilder[T, BasicStyle] = accum(_ + s + _)
  def accum(reducer: (String, String) => String) = new AccumStyleBuilder[T, BasicStyle](name, this, reducer)

  def delayed: DelayedStyleBuilder[T] = new DelayedStyleBuilder[T](name)
  def remove: RemoveStyleBuilder[T] = new RemoveStyleBuilder[T](name)
  def destroy: DestroyStyleBuilder[T] = new DestroyStyleBuilder[T](name)
}

final class DelayedStyleBuilder[T](val name: String) extends AnyVal with AttributeBuilder[T, DelayedStyle] {
  @inline private[outwatch] def assign(value: T) = DelayedStyle(name, value.toString)
}

final class RemoveStyleBuilder[T](val name: String) extends AnyVal with AttributeBuilder[T, RemoveStyle] {
  @inline private[outwatch] def assign(value: T) = RemoveStyle(name, value.toString)
}

final class DestroyStyleBuilder[T](val name: String) extends AnyVal with AttributeBuilder[T, DestroyStyle] {
  @inline private[outwatch] def assign(value: T) = DestroyStyle(name, value.toString)
}

final class AccumStyleBuilder[T, S <: BasicStyle](val name: String, builder: AttributeBuilder[T, S], reducer: (String, String) => String)
  extends AttributeBuilder[T, AccumStyle] {
  @inline private[outwatch] def assign(value: T) = AccumStyle(name, builder.assign(value), reducer)
}

// Keys

object KeyBuilder {
  def :=(key: Key.Value): IO[Key] = IO.pure(Key(key))
}

// Child / Children

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

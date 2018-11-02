package outwatch

import outwatch.dom.{CompositeModifier, ModifierStream, Observable, StringVNode, VDomModifier, IO}

trait AsVDomModifier[-T] {
  def asVDomModifier(value: T): VDomModifier
}

object AsVDomModifier {

  @inline def apply[T](implicit avm: AsVDomModifier[T]): AsVDomModifier[T] = avm

  implicit def seqModifier[T : AsVDomModifier](implicit vm: AsVDomModifier[T]): AsVDomModifier[Seq[T]] =
    (value: Seq[T]) => CompositeModifier(value.map(vm.asVDomModifier))

  implicit def optionModifier[T](implicit vm: AsVDomModifier[T]): AsVDomModifier[Option[T]] =
    (value: Option[T]) => value.fold(VDomModifier.empty)(vm.asVDomModifier)

  implicit object VDomModifierAsVDomModifier extends AsVDomModifier[VDomModifier] {
    def asVDomModifier(value: VDomModifier): VDomModifier = value
  }

  implicit object StringAsVDomModifier extends AsVDomModifier[String] {
    def asVDomModifier(value: String): VDomModifier = StringVNode(value)
  }

  implicit object IntAsVDomModifier extends AsVDomModifier[Int] {
    def asVDomModifier(value: Int): VDomModifier = StringVNode(value.toString)
  }

  implicit object DoubleAsVDomModifier extends AsVDomModifier[Double] {
    def asVDomModifier(value: Double): VDomModifier = StringVNode(value.toString)
  }

  implicit def observableRender[T](implicit r: AsVDomModifier[T]): AsVDomModifier[Observable[T]] = (valueStream: Observable[T]) =>
    ModifierStream(valueStream.map(r.asVDomModifier))

}
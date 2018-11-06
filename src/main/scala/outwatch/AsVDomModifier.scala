package outwatch

import monix.eval.TaskLike
import outwatch.dom.{CompositeModifier, IO, ModifierStream, Observable, StringVNode, VDomModifier}

import scala.scalajs.js

trait AsVDomModifier[-T] {
  def asVDomModifier(value: T): VDomModifier
}

object AsVDomModifier {

  @inline def apply[T](implicit avm: AsVDomModifier[T]): AsVDomModifier[T] = avm

  implicit def seqModifier[T : AsVDomModifier](implicit vm: AsVDomModifier[T]): AsVDomModifier[Seq[T]] =
    (value: Seq[T]) => value.map(vm.asVDomModifier).sequence.map(CompositeModifier)

  implicit def optionModifier[T](implicit vm: AsVDomModifier[T]): AsVDomModifier[Option[T]] =
    (value: Option[T]) => value.fold(VDomModifier.empty)(vm.asVDomModifier)

  implicit def undefOrModifier[T](implicit vm: AsVDomModifier[T]): AsVDomModifier[js.UndefOr[T]] =
    (value: js.UndefOr[T]) => value.fold(VDomModifier.empty)(vm.asVDomModifier)

  implicit object VDomModifierAsVDomModifier extends AsVDomModifier[VDomModifier] {
    def asVDomModifier(value: VDomModifier): VDomModifier = value
  }

  implicit object StringAsVDomModifier extends AsVDomModifier[String] {
    def asVDomModifier(value: String): VDomModifier = IO.pure(StringVNode(value))
  }

  implicit object IntAsVDomModifier extends AsVDomModifier[Int] {
    def asVDomModifier(value: Int): VDomModifier = IO.pure(StringVNode(value.toString))
  }

  implicit object DoubleAsVDomModifier extends AsVDomModifier[Double] {
    def asVDomModifier(value: Double): VDomModifier = IO.pure(StringVNode(value.toString))
  }

  implicit def observableRender[T](implicit r: AsVDomModifier[T]): AsVDomModifier[Observable[T]] = (valueStream: Observable[T]) =>
    IO.pure(ModifierStream(valueStream.concatMap(m => Observable.fromTaskLike(r.asVDomModifier(m)))))

  implicit def taskLikeRender[T, F[_]: TaskLike](implicit r: AsVDomModifier[T]): AsVDomModifier[F[T]] = (value: F[T]) =>
    Observable.fromTaskLike(value)
}
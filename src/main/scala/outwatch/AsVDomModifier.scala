package outwatch

import cats.effect.{IO, Sync}
import org.scalajs.dom.svg.G
import outwatch.dom.{ChildStreamReceiver, ChildrenStreamReceiver, CompositeModifier, Observable, StringModifier, VDomModifier, VDomModifierF, VNode, VNodeF}

trait AsVDomModifier[F[+_], -T] {
  def asVDomModifier(value: T)(implicit F: Sync[F]): VDomModifierF[F]
}

object AsVDomModifier {

  implicit def seqModifier[F[+_], T](implicit vm: AsVDomModifier[F, T]): AsVDomModifier[F, Seq[T]] =
    (value: Seq[T]) => value.map(vm.asVDomModifier).sequence.map(CompositeModifier)

  implicit def optionModifier[F[+_], T](implicit vm: AsVDomModifier[F, T]) = new AsVDomModifier[F, Option[T]] {
    def asVDomModifier(value: Option[T])(implicit F: Sync[F]): VDomModifierF[F] =
      value.map(vm.asVDomModifier).getOrElse(VDomModifier.empty)
  }
    (value: Option[T]) => value.map(vm.asVDomModifier) getOrElse VDomModifier.empty

  implicit def vDomModifierAsVDomModifier[F[+_]: Sync]: AsVDomModifier[F, VDomModifierF[F]] =
    new AsVDomModifier[F, VDomModifierF[F]] {
      def asVDomModifier(value: VDomModifierF[F])(implicit F: Sync[F]): VDomModifierF[F] = value
    }

  implicit def stringAsVDomModifier[F[+_]] = new AsVDomModifier[F, String] {
    def asVDomModifier(value: String)(implicit F: Sync[F]): VDomModifierF[F] = F.pure(StringModifier(value))
  }

  implicit def IntAsVDomModifier[F[+_]] = new AsVDomModifier[F, Int] {
    def asVDomModifier(value: Int)(implicit F: Sync[F]): VDomModifierF[F] = F.pure(StringModifier(value.toString))
  }

  implicit def DoubleAsVDomModifier[F[+_]] = new AsVDomModifier[F, Double] {
    def asVDomModifier(value: Double)(implicit F: Sync[F]): VDomModifierF[F] = F.pure(StringModifier(value.toString))
  }

  implicit def observableRender[F[+_]] = new AsVDomModifier[F, Observable[VNodeF[F]]] {
    def asVDomModifier(valueStream: Observable[VNodeF[F]])(implicit F: Sync[F]): VDomModifierF[F] =
      F.pure(ChildStreamReceiver[F](valueStream))
  }

  implicit def observableRender[F[+_], T: StaticVNodeRender]: AsVDomModifier[F, Observable[T]] =
    new AsVDomModifier[F, Observable[T]] {
      def asVDomModifier(valueStream: Observable[T])(implicit F: Sync[F]): VDomModifierF[F] = F.pure(
        ChildStreamReceiver[F](valueStream.map(implicitly[StaticVNodeRender[T]].render[F]))
      )
    }

  implicit def observableSeqRender[F[+_]] = new AsVDomModifier[F, Observable[Seq[VNodeF[F]]]] {
    def asVDomModifier(seqStream: Observable[Seq[VNodeF[F]]])(implicit F: Sync[F]): VDomModifierF[F] =
      F.pure(ChildrenStreamReceiver[F](seqStream))
  }

  implicit def observableSeqRender[F[+_], T: StaticVNodeRender] = new AsVDomModifier[F, Observable[Seq[T]]] {
    def asVDomModifier(seqStream: Observable[Seq[T]])(implicit F: Sync[F]): VDomModifierF[F] =
      F.pure(ChildrenStreamReceiver[F](seqStream.map(_.map(implicitly[StaticVNodeRender[T]].render[F]))))
  }

}
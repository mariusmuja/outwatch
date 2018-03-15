package outwatch

import cats.Applicative
import outwatch.dom.{ChildStreamReceiver, ChildrenStreamReceiver, CompositeModifier, Modifier, Observable, StringModifier, VDomModifierF}

trait AsVDomModifier[F[+_], -T] {
  def asVDomModifier(value: T)(implicit F: Applicative[F]): F[Modifier]
}

object AsVDomModifier {

  implicit def seqModifier[F[+_], A](implicit vm: AsVDomModifier[F, A]): AsVDomModifier[F, Seq[A]] =
    new AsVDomModifier[F, Seq[A]] {
      import cats.implicits._
      def asVDomModifier(value: Seq[A])(implicit F: Applicative[F]): F[Modifier] =
        value.toList.traverse(vm.asVDomModifier).map(CompositeModifier)
    }

  implicit def optionModifier[F[+_], A](implicit vm: AsVDomModifier[F, A]): AsVDomModifier[F, Option[A]] =
    new AsVDomModifier[F, Option[A]] {
      def asVDomModifier(value: Option[A])(implicit F: Applicative[F]): F[Modifier] =
        value.map(vm.asVDomModifier).getOrElse(VDomModifierF.empty[F])
    }

  implicit def vDomModifierAsVDomModifier[F[+_]]: AsVDomModifier[F, F[Modifier]] = new AsVDomModifier[F, F[Modifier]] {
      def asVDomModifier(value: F[Modifier])(implicit F: Applicative[F]): F[Modifier] = value
    }

  implicit def stringAsVDomModifier[F[+_]]: AsVDomModifier[F, String] = new AsVDomModifier[F, String] {
    def asVDomModifier(value: String)(implicit F: Applicative[F]): F[Modifier] = F.pure(StringModifier(value))
  }

  implicit def IntAsVDomModifier[F[+_]]: AsVDomModifier[F, Int] = new AsVDomModifier[F, Int] {
    def asVDomModifier(value: Int)(implicit F: Applicative[F]): F[Modifier] = F.pure(StringModifier(value.toString))
  }

  implicit def DoubleAsVDomModifier[F[+_]]: AsVDomModifier[F, Double] = new AsVDomModifier[F, Double] {
    def asVDomModifier(value: Double)(implicit F: Applicative[F]): F[Modifier] = F.pure(StringModifier(value.toString))
  }

//  implicit def observableRender[F[+_]]: AsVDomModifier[F, Observable[VNodeF[F]]] = new AsVDomModifier[F, Observable[VNodeF[F]]] {
//    def asVDomModifier(valueStream: Observable[VNodeF[F]])(implicit F: Effect[F]): F[Modifier] =
//      F.pure(ChildStreamReceiver[F](valueStream))
//  }

  implicit def observableRender2[F[+_], T](implicit R: StaticVNodeRender[F, T]): AsVDomModifier[F, Observable[T]] =
    new AsVDomModifier[F, Observable[T]] {
      def asVDomModifier(valueStream: Observable[T])(implicit F: Applicative[F]): F[Modifier] = F.pure(
        ChildStreamReceiver[F](valueStream.map(R.render))
      )
    }

//  implicit def observableSeqRender[F[+_]]: AsVDomModifier[F, Observable[Seq[VNodeF[F]]]]  = new AsVDomModifier[F, Observable[Seq[VNodeF[F]]]] {
//    def asVDomModifier(seqStream: Observable[Seq[VNodeF[F]]])(implicit F: Effect[F]): F[Modifier] =
//      F.pure(ChildrenStreamReceiver[F](seqStream))
//  }

  implicit def observableSeqRender2[F[+_], T](implicit R: StaticVNodeRender[F, T]): AsVDomModifier[F, Observable[Seq[T]]] = new AsVDomModifier[F, Observable[Seq[T]]] {
    def asVDomModifier(seqStream: Observable[Seq[T]])(implicit F: Applicative[F]): F[Modifier] =
      F.pure(ChildrenStreamReceiver[F](seqStream.map(_.map(R.render))))
  }

}
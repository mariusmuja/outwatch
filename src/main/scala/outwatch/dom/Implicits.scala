package outwatch.dom

import cats.effect.{Effect, Sync}
import com.raquo.domtypes.generic.keys
import outwatch.AsVDomModifier
import outwatch.dom.helpers.BasicStyleBuilder

trait Implicits {

  implicit def asVDomModifier[F[+_]: Effect, T](value: T)(implicit vm: AsVDomModifier[F, T]): F[Modifier] =
    vm.asVDomModifier(value)

  implicit class ioVTreeMerge[F[+_]: Sync](vnode: F[VTree[F]]) {
    import cats.implicits._
    def apply(args: F[Modifier]*): F[VTree[F]] = vnode.flatMap(_.apply(args: _*))
  }

  implicit def StyleIsBuilder[T](style: keys.Style[T]): BasicStyleBuilder[T] = new BasicStyleBuilder[T](style.cssName)

}

package outwatch

import cats.Applicative
import outwatch.dom.{StaticVNode, StringVNode, VNodeF}

trait StaticVNodeRender[F[+_], -T] {
  def render(value: T)(implicit A: Applicative[F]): F[StaticVNode]
}

object StaticVNodeRender {

  implicit def vNodeaRender[F[+_], T]: StaticVNodeRender[F, VNodeF[F]] =
    new StaticVNodeRender[F, VNodeF[F]] {
      def render(value: VNodeF[F])(implicit A: Applicative[F]): F[StaticVNode] = value
    }

  implicit def optionRender[F[+_], T](implicit svnr: StaticVNodeRender[F, T]): StaticVNodeRender[F, Option[T]] =
    new StaticVNodeRender[F, Option[T]] {
      def render(value: Option[T])(implicit A: Applicative[F]): F[StaticVNode] =
        value.fold(A.pure(StaticVNode.empty))(svnr.render)
    }


  implicit def stringRender[F[+_]]: StaticVNodeRender[F, String] = new StaticVNodeRender[F, String] {
    def render(value: String)(implicit A: Applicative[F]): F[StaticVNode] =
      A.pure(StringVNode(value))
  }

  implicit def intRender[F[+_]] : StaticVNodeRender[F, Int] = new StaticVNodeRender[F, Int] {
    def render(value: Int)(implicit A: Applicative[F]): F[StaticVNode] =
      A.pure(StringVNode(value.toString))
  }

  implicit def doubleRender[F[+_]]: StaticVNodeRender[F, Double] = new StaticVNodeRender[F, Double] {
    def render(value: Double)(implicit A: Applicative[F]): F[StaticVNode] =
      A.pure(StringVNode(value.toString))
  }
}

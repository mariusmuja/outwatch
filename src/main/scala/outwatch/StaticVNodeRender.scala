package outwatch

import cats.effect.IO
import outwatch.dom.{StaticVNode, StringVNode, VNode}

trait StaticVNodeRender[T] {
  def render(value: T): IO[StaticVNode]
}

object StaticVNodeRender {

  implicit object NodeRenderer extends StaticVNodeRender[VNode] {
    def render(value: VNode): IO[StaticVNode] = value
  }

  implicit object StringRenderer extends StaticVNodeRender[String] {
    def render(value: String): IO[StaticVNode] = IO.pure(StringVNode(value))
  }

  implicit object IntRenderer extends StaticVNodeRender[Int] {
    def render(value: Int): IO[StaticVNode] = IO.pure(StringVNode(value.toString))
  }

  implicit object DoubleRenderer extends StaticVNodeRender[Double] {
    def render(value: Double): IO[StaticVNode] = IO.pure(StringVNode(value.toString))
  }

}
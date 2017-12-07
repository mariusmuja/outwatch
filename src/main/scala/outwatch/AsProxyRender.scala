package outwatch

import cats.effect.IO
import outwatch.dom.{StaticVNode, StringVNode, VNode}

trait AsProxyRender[T] {
  def render(value: T): IO[StaticVNode]
}

object AsProxyRender {

  implicit object NodeRenderer extends AsProxyRender[VNode] {
    def render(value: VNode): IO[StaticVNode] = value
  }

  implicit object StringRenderer extends AsProxyRender[String] {
    def render(value: String): IO[StaticVNode] = IO.pure(StringVNode(value))
  }

  implicit object IntRenderer extends AsProxyRender[Int] {
    def render(value: Int): IO[StaticVNode] = IO.pure(StringVNode(value.toString))
  }

  implicit object DoubleRenderer extends AsProxyRender[Double] {
    def render(value: Double): IO[StaticVNode] = IO.pure(StringVNode(value.toString))
  }

}
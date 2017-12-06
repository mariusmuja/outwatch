package outwatch

import cats.effect.IO

import scala.language.implicitConversions


package object dom extends Attributes with Tags with HandlerFactories {

  type VNode = IO[VNode_]
  type VDomModifier = IO[VDomModifier_]

  type Observable[+A] = monix.reactive.Observable[A]
  val Observable = monix.reactive.Observable

  type Sink[-A] = outwatch.Sink[A]
  val Sink = outwatch.Sink

  type Pipe[-I, +O] = outwatch.Pipe[I, O]
  val Pipe = outwatch.Pipe

  type Handler[T] = outwatch.Handler[T]
  val Handler = outwatch.Handler

  val EmptyVNode = IO.pure(VTree("", Seq.empty))

  implicit def renderVNode[T](value: T)(implicit vnr: VNodeRender[T]): VNode = vnr.render(value)

  implicit def optionIsEmptyModifier(opt: Option[VDomModifier]): VDomModifier = opt getOrElse IO.pure(EmptyVDomModifier)

  implicit def compositeModifier(modifiers: Seq[VDomModifier]): VDomModifier = IO.pure(CompositeVDomModifier(modifiers))

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode= {
      vnode.flatMap {
        case vtree: VTree => vtree(args: _*)
        case sn: StringNode => IO.pure(sn)
      }
    }
  }

}

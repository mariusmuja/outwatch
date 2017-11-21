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

  implicit def stringNode(string: String): VDomModifier = IO.pure(StringNode(string))

  implicit def optionIsEmptyModifier(opt: Option[VDomModifier]): VDomModifier = opt getOrElse IO.pure(EmptyVDomModifier)

  implicit def seqModifier(modifiers: Seq[VDomModifier]): VDomModifier = IO.pure(CompositeModifier(modifiers))

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = {
      vnode.flatMap(vnode_ => vnode_(args:_*))
    }
  }

  def stl(property:String) = new helpers.StyleBuilder(property)
}

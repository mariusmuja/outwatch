package outwatch

import cats.effect.IO

import scala.language.implicitConversions


package object dom extends Attributes with Tags with Handlers {

  type Observable[+A] = monix.reactive.Observable[A]
  val Observable = monix.reactive.Observable

  type Sink[-A] = outwatch.Sink[A]
  val Sink = outwatch.Sink

  type VNode = IO[VNode_]
  type VDomModifier = IO[VDomModifier_]

  implicit def stringNode(string: String): VDomModifier = IO.pure(StringNode(string))

  implicit def optionIsEmptyModifier(opt: Option[VDomModifier]): VDomModifier = opt getOrElse IO.pure(EmptyVDomModifier)

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = {
      vnode.flatMap(vnode_ => vnode_(args:_*))
    }
  }

  def stl(property:String) = new helpers.StyleBuilder(property)
}

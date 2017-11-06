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

  implicit class ioVTreeMerge(tree: IO[VTree]) {
    def apply(args: VDomModifier*): IO[VTree] = {
      tree.flatMap(vtree => IO.pure(VTree(vtree.nodeType, vtree.modifiers ++ args)))
    }
  }

  def stl(property:String) = new helpers.StyleBuilder(property)
}

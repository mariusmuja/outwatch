package outwatch.dom

import cats.syntax.apply._
import com.raquo.domtypes.generic.keys
import outwatch.AsVDomModifier
import outwatch.dom.helpers.BasicStyleBuilder

import scala.collection.mutable.ArrayBuffer

trait Implicits {

  implicit def asVDomModifier[T](value: T)(implicit vm: AsVDomModifier[T]): VDomModifier = vm.asVDomModifier(value)

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = vnode.flatMap(_.apply(args: _*))
  }

  implicit def StyleIsBuilder[T](style: keys.Style[T]): BasicStyleBuilder[T] = new BasicStyleBuilder[T](style.cssName)

  private[outwatch] implicit class SeqIOSequence[T](args: Seq[IO[T]]) {
    val sequence: IO[Seq[T]] = {
      if (args.isEmpty) IO.pure(Seq.empty)
      else if (args.lengthCompare(1) == 0) args.head.map(t => Seq(t))
      else args.foldLeft(
        IO {
          val empty = ArrayBuffer.empty[T]
          empty.sizeHint(args.length)
          empty
        }
      )((a, l) => a.map2(l)(_ += _))
    }
  }

}
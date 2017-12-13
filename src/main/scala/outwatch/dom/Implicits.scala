package outwatch.dom

import com.raquo.domtypes.generic.keys
import outwatch.ValueModifier
import outwatch.dom.helpers.BasicStyleBuilder

trait Implicits {

  implicit def valueModifier[T](value: T)(implicit mr: ValueModifier[T]): VDomModifier = mr.asModifier(value)

  implicit def optionIsEmptyModifier(opt: Option[VDomModifier]): VDomModifier = opt getOrElse VDomModifier.empty

  implicit def compositeModifier(modifiers: Seq[VDomModifier]): VDomModifier = VDomModifier.apply(modifiers : _*)

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = vnode.flatMap(_(args:_*))
  }

  implicit def StyleIsBuilder[T](style: keys.Style[T]): BasicStyleBuilder[T] = new BasicStyleBuilder[T](style.cssName)
}

package outwatch.dom

import com.raquo.domtypes.generic.keys
import outwatch.ValueModifier
import outwatch.dom.helpers.StyleBuilder

trait Implicits {

  implicit def valueModifier[T](value: T)(implicit mr: ValueModifier[T]): VDomModifier = mr.asModifier(value)

  implicit def optionIsEmptyModifier(opt: Option[VDomModifier]): VDomModifier = opt getOrElse VDomModifier.empty

  implicit def compositeModifier(modifiers: Seq[VDomModifier]): VDomModifier = modifiers.sequence.map(CompositeModifier)

  implicit class ioVTreeMerge(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = {
      vnode.flatMap(vnode_ => vnode_(args:_*))
    }
  }

  implicit def StyleIsBuilder[T](style: keys.Style[T]): StyleBuilder[T] = new StyleBuilder[T](style.cssName)
}

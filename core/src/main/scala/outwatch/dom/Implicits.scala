package outwatch.dom

import com.raquo.domtypes.generic.keys
import outwatch.AsVDomModifier
import outwatch.dom.helpers.BasicStyleBuilder

trait Implicits {

  implicit def asVDomModifier[T: AsVDomModifier](value: T): VDomModifier = AsVDomModifier[T].asVDomModifier(value)

  implicit class VNodeExt(vnode: VNode) {
    def apply(args: VDomModifier*): VNode = vnode.flatMap(_.apply(args: _*))
  }

  implicit def StyleIsBuilder[T](style: keys.Style[T]): BasicStyleBuilder[T] = new BasicStyleBuilder[T](style.cssName)

}
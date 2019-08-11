package snabbdom

import org.scalajs.dom._

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.|

@js.native
@JSImport("snabbdom/h", JSImport.Namespace, globalFallback = "h")
object hProvider extends js.Object {
  val default: hFunction = js.native
}

@js.native
trait hFunction extends js.Any {
  def apply(nodeType: String, dataObject: DataObject): VNodeProxy = js.native
  def apply(nodeType: String, dataObject: DataObject, text: String): VNodeProxy = js.native
  def apply(nodeType: String, dataObject: DataObject, children: js.UndefOr[js.Array[VNodeProxy]]): VNodeProxy = js.native
}

object hFunction {
  def apply(nodeType: String, dataObject: DataObject): VNodeProxy = {
    hProvider.default.apply(nodeType, dataObject, js.undefined)
  }
  def apply(nodeType: String, dataObject: DataObject, text: String): VNodeProxy = {
    hProvider.default.apply(nodeType, dataObject, text)
  }
  def apply(nodeType: String, dataObject: DataObject, children: js.Array[VNodeProxy]): VNodeProxy = {
    val nonEmpty: js.UndefOr[js.Array[VNodeProxy]] = if (children.isEmpty) js.undefined else children
    hProvider.default.apply(nodeType, dataObject, nonEmpty)
  }
}


trait Hooks extends js.Object {
  val insert: js.UndefOr[Hooks.HookSingleFn]
  val prepatch: js.UndefOr[Hooks.HookPairFn]
  val update: js.UndefOr[Hooks.HookPairFn]
  val postpatch: js.UndefOr[Hooks.HookPairFn]
  val destroy: js.UndefOr[Hooks.HookSingleFn]
}

object Hooks {
  type HookSingleFn = js.Function1[VNodeProxy, Unit]
  type HookPairFn = js.Function2[VNodeProxy, VNodeProxy, Unit]

  def apply(
    insert: js.UndefOr[HookSingleFn] = js.undefined,
    prepatch: js.UndefOr[HookPairFn] = js.undefined,
    update: js.UndefOr[HookPairFn] = js.undefined,
    postpatch: js.UndefOr[HookPairFn] = js.undefined,
    destroy: js.UndefOr[HookSingleFn] = js.undefined
  ): Hooks = {

    literal(
      insert = insert,
      prepatch = prepatch,
      update = update,
      postpatch = postpatch,
      destroy = destroy
    ).asInstanceOf[Hooks]
  }
}


trait DataObject extends js.Object {
  import DataObject._

  val attrs: js.Dictionary[AttrValue]
  val props: js.Dictionary[PropValue]
  val style: js.Dictionary[StyleValue]
  val on: js.Dictionary[js.Function1[Event, Unit]]
  val hook: Hooks
  val key: js.UndefOr[KeyValue]
}

object DataObject {

  type PropValue = Any
  type AttrValue = String | Boolean
  type StyleValue = String | js.Dictionary[String]
  type KeyValue = String | Double  // https://github.com/snabbdom/snabbdom#key--string--number

  def apply(attrs: js.UndefOr[js.Dictionary[AttrValue]],
            on: js.UndefOr[js.Dictionary[js.Function1[Event, Unit]]],
            hooks: js.UndefOr[Hooks] = js.undefined
           ): DataObject = apply(attrs, js.undefined, js.undefined, on, hooks, js.undefined)


  def apply(attrs: js.UndefOr[js.Dictionary[AttrValue]],
            props: js.UndefOr[js.Dictionary[PropValue]],
            style: js.UndefOr[js.Dictionary[StyleValue]],
            on: js.UndefOr[js.Dictionary[js.Function1[Event, Unit]]],
            hook: js.UndefOr[Hooks],
            key: js.UndefOr[KeyValue]
           ): DataObject = {

    literal(
      attrs = attrs,
      props = props,
      style = style,
      on = on,
      hook = hook,
      key = key.asInstanceOf[js.Any]
    ).asInstanceOf[DataObject]
  }
}

object patch {

  private lazy val p = Snabbdom.init(js.Array(
    SnabbdomClass.default,
    SnabbdomEventListeners.default,
    SnabbdomAttributes.default,
    SnabbdomCustomProps.default,
    SnabbdomStyle.default
  ))

  def apply(firstNode: VNodeProxy, vNode: VNodeProxy): VNodeProxy = p(firstNode, vNode)

  def apply(firstNode: org.scalajs.dom.Element, vNode: VNodeProxy): VNodeProxy = p(firstNode, vNode)
}


trait VNodeProxy extends js.Object {
  var sel: String
  var data: DataObject
  var children: js.UndefOr[js.Array[VNodeProxy]] = js.undefined
  var elm: js.UndefOr[Element] = js.undefined
  var text: js.UndefOr[String] = js.undefined
  var key: js.UndefOr[DataObject.KeyValue] = js.undefined
}

object VNodeProxy {
  def fromString(string: String): VNodeProxy = string.asInstanceOf[VNodeProxy]

  implicit class VNodeProxyExt(vproxy: VNodeProxy) {
    def copyFrom(newProxy: VNodeProxy): VNodeProxy = {
      vproxy.children = newProxy.children
      vproxy.text = newProxy.text
      vproxy.data = newProxy.data
      vproxy.elm = newProxy.elm
      vproxy.key = newProxy.key
      vproxy.sel = newProxy.sel

      vproxy
    }
  }
}


@js.native
@JSImport("snabbdom", JSImport.Namespace, globalFallback = "snabbdom")
object Snabbdom extends js.Object {
  def init(args: js.Array[Any]): js.Function2[Node | VNodeProxy, VNodeProxy, VNodeProxy] = js.native

}

@js.native
@JSImport("snabbdom/modules/class", JSImport.Namespace, globalFallback = "snabbdom_class")
object SnabbdomClass extends js.Object {
  val default: js.Any = js.native
}

@js.native
@JSImport("snabbdom/modules/eventlisteners", JSImport.Namespace, globalFallback = "snabbdom_eventlisteners")
object SnabbdomEventListeners extends js.Object{
  val default: js.Any = js.native
}

@js.native
@JSImport("snabbdom/modules/attributes", JSImport.Namespace, globalFallback = "snabbdom_attributes")
object SnabbdomAttributes extends js.Object{
  val default: js.Any = js.native
}

// forked snabbdom-props module where the ground-thruth is the dom
@js.native
@JSImport("./snabbdom-custom-props", JSImport.Namespace)
object SnabbdomCustomProps extends js.Object {
  val default: js.Any = js.native
}

// original snabbdom-props
// @js.native
// @JSImport("snabbdom/modules/props", JSImport.Namespace, globalFallback = "snabbdom_props")
// object SnabbdomProps extends js.Object{
//   val default: js.Any = js.native
// }


@js.native
@JSImport("snabbdom/modules/style", JSImport.Namespace, globalFallback = "snabbdom_style")
object SnabbdomStyle extends js.Object {
  val default: js.Any = js.native
}

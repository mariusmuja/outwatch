package outwatch.dom.helpers

import monix.execution.Scheduler
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._


private[outwatch] trait SnabbdomStyles { self: SeparatedStyles =>
  def toSnabbdom: js.Dictionary[Style.Value] = {
    if (delayedDict.nonEmpty) styleDict("delayed") = delayedDict.get : Style.Value
    if (removeDict.nonEmpty) styleDict("remove") = removeDict.get : Style.Value
    if (destroyDict.nonEmpty) styleDict("destroy") = destroyDict.get : Style.Value

    styleDict
  }
}

private[outwatch] trait SnabbdomHooks { self: SeparatedHooks =>

  @inline private def createHookSingle(hooks: js.UndefOr[js.Array[_ <: Hook[dom.Element]]], lifecycleHooks: js.UndefOr[js.Array[_ <: LifecycleHook]]
  )(implicit s: Scheduler): js.UndefOr[Hooks.HookSingleFn] = {
    if (hooks.nonEmpty || lifecycleHooks.nonEmpty) { p: VNodeProxy =>
      if (hooks.nonEmpty) for (e <- p.elm) hooks.get.foreach(_.observer.feed(e :: Nil))
      if (lifecycleHooks.nonEmpty) lifecycleHooks.get.foreach(_.fn(p, s))
    }: Hooks.HookSingleFn
    else js.undefined
  }

  @inline private def createHookPair(hooks: Seq[Hook[(dom.Element, dom.Element)]])(implicit s: Scheduler): Hooks.HookPairFn = {
    (old: VNodeProxy, cur: VNodeProxy) =>
      for (o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.feed((o, c) :: Nil))
  }

  @inline private def createHookPairOption(hooks: Seq[Hook[(Option[dom.Element], Option[dom.Element])]])(implicit s: Scheduler): Hooks.HookPairFn = {
    (old: VNodeProxy, cur: VNodeProxy) =>
      hooks.foreach(_.observer.feed((old.elm.toOption, cur.elm.toOption)::Nil))
  }

  def toSnabbdom(implicit s: Scheduler): Hooks = {
    val insertHook = createHookSingle(insertHooks, insertProxyHooks)
    val prePatchHook = prePatchHooks.flatMap(hooks => createHookPairOption(hooks))
    val updateHook = updateHooks.flatMap(hooks => createHookPair(hooks))
    val postPatchHook = postPatchHooks.flatMap(hooks => createHookPair(hooks))
    val destroyHook = createHookSingle(destroyHooks, destroyProxyHooks)

    Hooks(insertHook, prePatchHook, updateHook, postPatchHook, destroyHook)
  }
}


private[outwatch] trait SnabbdomEmitters { self: SeparatedEmitters =>

  @inline private def emittersToFunction(emitters: js.Array[Emitter]): js.Function1[dom.Event, Unit] = {
    event => emitters.foreach(_.trigger(event))
  }

  def toSnabbdom: js.Dictionary[js.Function1[dom.Event, Unit]] =
    emitters.groupBy(_.eventType).mapValues(emittersToFunction).toJSDictionary
}

private[outwatch] trait SnabbdomModifiers { self: SeparatedModifiers =>

  private[outwatch] def toSnabbdom(nodeType: String)(implicit scheduler: Scheduler): VNodeProxy = {

    val dataObject =  DataObject(
      attributes.flatMap(_.attrs), attributes.flatMap(_.props), attributes.flatMap(_.styles.map(_.toSnabbdom)),
      emitters.map(_.toSnabbdom), hooks.map(_.toSnabbdom), key.map(_.value)
    )

    if (nodes.isEmpty) {
      hFunction(nodeType, dataObject)
    } else {
      if (hasVtrees) {
        val proxies: js.Array[VNodeProxy] = nodes.map(_.toSnabbdom)
        hFunction(nodeType, dataObject, proxies)
      } else {
        hFunction(nodeType, dataObject, nodes.map(_.asInstanceOf[StringVNode].string).mkString)
      }
    }
  }
}

private[outwatch] trait SnabbdomState { self: VNodeState =>
  private[outwatch] def toSnabbdom(nodeType: String)(implicit scheduler: Scheduler): VNodeProxy = {
    initial.toSnabbdom(nodeType)
  }
}
package outwatch.dom.helpers

import monix.execution.Scheduler
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._


private[outwatch] object SnabbdomModifiers {

  @inline private def createStyles(styles: SeparatedStyles): js.Dictionary[Style.Value] = {
    import styles._

    if (delayedDict.nonEmpty) styleDict("delayed") = delayedDict.get : Style.Value
    if (removeDict.nonEmpty) styleDict("remove") = removeDict.get : Style.Value
    if (destroyDict.nonEmpty) styleDict("destroy") = destroyDict.get : Style.Value

    styleDict
  }


  @inline private def emittersToFunction(emitters: js.Array[Emitter]): js.Function1[dom.Event, Unit] = {
    event => emitters.foreach(_.trigger(event))
  }

  @inline private def createEvents(emitters: js.Array[Emitter]): js.Dictionary[js.Function1[dom.Event, Unit]] =
    emitters.groupBy(_.eventType).mapValues(emittersToFunction).toJSDictionary



  @inline private def createHookSingle(hooks: js.UndefOr[js.Array[_ <: Hook[dom.Element]]], lifecycleHooks: js.UndefOr[js.Array[_ <: LifecycleHook]]
  )(implicit s: Scheduler): js.UndefOr[Hooks.HookSingleFn] = {
    if (hooks.nonEmpty || lifecycleHooks.nonEmpty) { p: VNodeProxy =>
      if (hooks.nonEmpty) for (e <- p.elm) hooks.get.foreach(_.observer.feed(e :: Nil))
      lifecycleHooks.foreach(_.foreach(_.fn(p, s)))
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

  @inline private def createHooks(hooks: SeparatedHooks)(implicit s: Scheduler): Hooks = {
    import hooks._

    val insertHook = createHookSingle(insertHooks, insertLifecycleHooks)
    val prePatchHook = prePatchHooks.flatMap(hooks => createHookPairOption(hooks))
    val updateHook = updateHooks.flatMap(hooks => createHookPair(hooks))
    val postPatchHook = postPatchHooks.flatMap(hooks => createHookPair(hooks))
    val destroyHook = createHookSingle(destroyHooks, destroyLifecycleHooks)

    Hooks(insertHook, prePatchHook, updateHook, postPatchHook, destroyHook)
  }


  private[outwatch] def toSnabbdom(nodeType: String, modifiers: SimpleModifiers)(implicit scheduler: Scheduler): VNodeProxy = {

    val dataObject =  DataObject(
      modifiers.attrs,
      modifiers.props,
      modifiers.styles.map(createStyles),
      modifiers.emitters.map(createEvents),
      modifiers.hooks.map(createHooks),
      modifiers.key.map(_.value)
    )

    if (modifiers.nodes.isEmpty) {
      hFunction(nodeType, dataObject)
    } else {
      if (modifiers.hasVtrees) {
        val proxies: js.Array[VNodeProxy] = modifiers.nodes.map(_.toSnabbdom)
        hFunction(nodeType, dataObject, proxies)
      } else {
        hFunction(nodeType, dataObject, modifiers.nodes.map(_.asInstanceOf[StringVNode].string).mkString)
      }
    }
  }

  def toProxy(nodeType: String, modifiers: Array[Modifier])(implicit scheduler: Scheduler): VNodeProxy = {
    toSnabbdom(nodeType, StreamHandler.from(modifiers))
  }
}
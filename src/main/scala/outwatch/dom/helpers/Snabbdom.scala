package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.Scheduler
import monix.execution.cancelables.SingleAssignCancelable
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._


private[outwatch] trait SnabbdomStyles { self: SeparatedStyles =>
  def toSnabbdom: js.Dictionary[Style.Value] = {
    val styleDict = js.Dictionary[Style.Value]()

    val delayedDict = js.Dictionary[String]()
    val removeDict = js.Dictionary[String]()
    val destroyDict = js.Dictionary[String]()

    styles.foreach {
      case s: BasicStyle => styleDict(s.title) = s.value
      case s: DelayedStyle => delayedDict(s.title) = s.value
      case s: RemoveStyle => removeDict(s.title) = s.value
      case s: DestroyStyle => destroyDict(s.title) = s.value
      case a: AccumStyle =>
        styleDict(a.title) = styleDict.get(a.title)
          .fold[Style.Value](a.value)(s => a.accum(s.asInstanceOf[String], a.value))
    }

    if (delayedDict.nonEmpty) styleDict("delayed") = delayedDict : Style.Value
    if (removeDict.nonEmpty) styleDict("remove") = removeDict : Style.Value
    if (destroyDict.nonEmpty) styleDict("destroy") = destroyDict : Style.Value

    styleDict
  }
}

private[outwatch] trait SnabbdomAttributes { self: SeparatedAttributes =>

  def toSnabbdom: (js.Dictionary[Attr.Value], js.Dictionary[Prop.Value], js.Dictionary[Style.Value]) = {
    val attrsDict = js.Dictionary[Attr.Value]()
    val propsDict = js.Dictionary[Prop.Value]()

    attrs.foreach {
      case a: BasicAttr => attrsDict(a.title) = a.value
      case a: AccumAttr => attrsDict(a.title) = attrsDict.get(a.title).fold(a.value)(a.accum(_, a.value))
    }
    props.foreach { p => propsDict(p.title) = p.value }

    (attrsDict, propsDict, styles.toSnabbdom)
  }
}

private[outwatch] trait SnabbdomHooks { self: SeparatedHooks =>

  private def createHookSingle(hooks: Seq[Hook[dom.Element]]): js.UndefOr[Hooks.HookSingleFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookSingleFn](hooks =>
      (p: VNodeProxy) => for (e <- p.elm) hooks.foreach(_.observer.onNext(e))
    ).orUndefined
  }

  private def createHookPair(hooks: Seq[Hook[(dom.Element, dom.Element)]]): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => for (o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.onNext((o, c)))
    ).orUndefined
  }

  private def createHookPairOption(hooks: Seq[Hook[(Option[dom.Element], Option[dom.Element])]]
  ): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => hooks.foreach(_.observer.onNext((old.elm.toOption, cur.elm.toOption)))
    ).orUndefined
  }

  private def createInsertHook(
    streams: Streams,
    subscription: SingleAssignCancelable,
    hooks: Seq[InsertHook]
  )(implicit s: Scheduler): Hooks.HookSingleFn = (vproxy: VNodeProxy) => {

    def patchProxy(prev: VNodeProxy, modifiers: SeparatedModifiers): VNodeProxy = {
      val proxy = modifiers.toSnabbdom(prev.sel, Streams(Observable.empty), Some(prev))
      patch(prev, proxy)
    }

    subscription := streams.observable
      .scan(vproxy)(patchProxy)
      .subscribe(
        _ => Continue,
        e => dom.console.error(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
      )

    vproxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
  }


  private def createDestroyHook(
    subscription: SingleAssignCancelable,
    hooks: Seq[DestroyHook]
  ): Hooks.HookSingleFn = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.cancel()
    ()
  }

  def toSnabbdom(streams: Streams)(implicit s: Scheduler): Hooks = {
    val (insertHook, destroyHook) = if (streams.nonEmpty) {
      val subscription = SingleAssignCancelable()
      val insertHook: js.UndefOr[Hooks.HookSingleFn] = createInsertHook(streams, subscription, insertHooks)
      val destroyHook: js.UndefOr[Hooks.HookSingleFn] = createDestroyHook(subscription, destroyHooks)
      (insertHook, destroyHook)
    }
    else {
      val insertHook = createHookSingle(insertHooks)
      val destroyHook = createHookSingle(destroyHooks)
      (insertHook, destroyHook)
    }
    val prePatchHook = createHookPairOption(prePatchHooks)
    val updateHook = createHookPair(updateHooks)
    val postPatchHook = createHookPair(postPatchHooks)

    Hooks(insertHook, prePatchHook, updateHook, postPatchHook, destroyHook)
  }
}

private[outwatch] trait SnabbdomEmitters { self: SeparatedEmitters =>

  private def emittersToFunction(emitters: Seq[Emitter]): js.Function1[dom.Event, Unit] = {
    event => emitters.foreach(_.trigger(event))
  }

  def toSnabbdom: js.Dictionary[js.Function1[dom.Event, Unit]] = {
    emitters
      .groupBy(_.eventType)
      .mapValues(emittersToFunction)
      .toJSDictionary
  }
}

private[outwatch] trait SnabbdomModifiers { self: SeparatedModifiers =>

  private def createDataObject(obj: Option[DataObject], streams: Streams)(implicit s: Scheduler): DataObject = {

    val (attrs, props, style) = attributes.toSnabbdom
    val eventHandlers = emitters.toSnabbdom
    val snbHooks = hooks.toSnabbdom(streams)

    val key = keys.lastOption.map(_.value)
      .orElse(obj.flatMap(_.key.toOption))
      .orElse(Option(streams).filter(_.nonEmpty).map[Key.Value](_.hashCode))

    DataObject(attrs, props, style, eventHandlers, snbHooks, key.orUndefined)
  }

  private[outwatch] def toSnabbdom(
    nodeType: String, streams: Streams, previousProxy: Option[VNodeProxy] = None
  )(implicit scheduler: Scheduler): VNodeProxy = {

    val dataObject = createDataObject(previousProxy.map(_.data), streams)

    children match {
      case Children.VNodes(vnodes) =>
        val proxies = vnodes.map(_.toSnabbdom)
        hFunction(nodeType, dataObject, proxies.toJSArray)
      case Children.StringModifiers(textChildren) =>
        hFunction(nodeType, dataObject, textChildren.map(_.string).mkString)
      case Children.Empty =>
        hFunction(nodeType, dataObject)
    }
  }
}

private[outwatch] trait SnabbdomState { self: VNodeState =>
  private[outwatch] def toSnabbdom(nodeType: String)(implicit scheduler: Scheduler): VNodeProxy = {
    modifiers.toSnabbdom(nodeType, Streams(stream))
  }
}
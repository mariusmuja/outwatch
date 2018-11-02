package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._


private[outwatch] trait SnabbdomStyles { self: SeparatedStyles =>
  def toSnabbdom: js.Dictionary[Style.Value] = {
    if (delayedDict.nonEmpty) styleDict("delayed") = delayedDict : Style.Value
    if (removeDict.nonEmpty) styleDict("remove") = removeDict : Style.Value
    if (destroyDict.nonEmpty) styleDict("destroy") = destroyDict : Style.Value

    styleDict
  }
}

private[outwatch] trait SnabbdomAttributes { self: SeparatedAttributes =>

  def toSnabbdom: (js.Dictionary[Attr.Value], js.Dictionary[Prop.Value], js.Dictionary[Style.Value]) = {
    (attrs, props, styles.toSnabbdom)
  }
}

private[outwatch] trait SnabbdomHooks { self: SeparatedHooks =>

  @inline private def createHookSingle(hooks: Seq[Hook[dom.Element]]): js.UndefOr[Hooks.HookSingleFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookSingleFn](hooks =>
      (p: VNodeProxy) => for (e <- p.elm) hooks.foreach(_.observer.onNext(e))
    ).orUndefined
  }

  @inline private def createHookPair(hooks: Seq[Hook[(dom.Element, dom.Element)]]): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => for (o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.onNext((o, c)))
    ).orUndefined
  }

  @inline private def createHookPairOption(hooks: Seq[Hook[(Option[dom.Element], Option[dom.Element])]]
  ): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => hooks.foreach(_.observer.onNext((old.elm.toOption, cur.elm.toOption)))
    ).orUndefined
  }

  private def createInsertHook(
    streams: Streams,
    subscription: AssignableCancelable,
    hooks: Seq[InsertHook]
  )(implicit s: Scheduler): Hooks.HookSingleFn = (vproxy: VNodeProxy) => {

    def patchProxy(prev: VNodeProxy, modifiers: SeparatedModifiers): VNodeProxy = {
      val proxy = modifiers.toSnabbdom(prev.sel, Streams.empty, Some(prev))
      patch(prev, proxy)
    }

    subscription := streams.observable
      .scan(vproxy)(patchProxy)
      .subscribe(
        { newProxy =>
          vproxy.copyFrom(newProxy)
          Continue
        },
        e => dom.console.error(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
      )

    vproxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
  }


  private def createDestroyHook(
    subscription: AssignableCancelable,
    hooks: Seq[DestroyHook]
  ): Hooks.HookSingleFn = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.cancel()
    ()
  }

  private case class AssignableCancelable() extends Cancelable {
    private var inner: Option[Cancelable] = None

    def :=(c: Cancelable): Unit = {
      inner.foreach(_.cancel())
      inner = Some(c)
    }

    override def cancel(): Unit = {
      inner.foreach(_.cancel())
      inner = None
    }
  }

  def toSnabbdom(streams: Streams)(implicit s: Scheduler): Hooks = {
    val (insertHook, destroyHook) = if (streams.nonEmpty) {
      val subscription = AssignableCancelable()
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

  private def emittersToFunction(emitters: js.Array[Emitter]): js.Function1[dom.Event, Unit] = {
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

  private def createDataObject(prev: Option[DataObject], streams: Streams)(implicit s: Scheduler): DataObject = {

    val (attrs, props, style) = attributes.toSnabbdom
    val eventHandlers = emitters.toSnabbdom
    //if (prev.nonEmpty && streams.nonEmpty) assert(false) // Cannot have prev proxy and non-empty streams


    val snbHooks = prev.fold(hooks.toSnabbdom(streams))(_.hook)

    val key = keyOption.map(_.value)
      .orElse(prev.flatMap(_.key.toOption))
      .orElse(Option(streams).filter(_.nonEmpty).map[Key.Value](_.hashCode))

    DataObject(attrs, props, style, eventHandlers, snbHooks, key.orUndefined)
  }

  private[outwatch] def toSnabbdom(
    nodeType: String, streams: Streams, prev: Option[VNodeProxy] = None
  )(implicit scheduler: Scheduler): VNodeProxy = {

    val dataObject = createDataObject(prev.map(_.data), streams)

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
    initial.toSnabbdom(nodeType, Streams(stream))
  }
}
package outwatch.dom.helpers

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Scheduler
import monix.execution.cancelables.SingleAssignCancelable
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.JSON




private[outwatch] object DictionaryOps {
  def merge[T](first: js.Dictionary[T], second: js.Dictionary[T]) = {
    val result = js.Dictionary.empty[T]
    first.foreach { case (key, value) => result(key) = value }
    second.foreach { case (key, value) => result(key) = value }
    result
  }
}


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
        styleDict(a.title) = styleDict.get(a.title).map(s =>
          a.accum(s.asInstanceOf[String], a.value): Style.Value
        ).getOrElse(a.value)

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
      case a: AccumAttr => attrsDict(a.title) = attrsDict.get(a.title).map(a.accum(_, a.value)).getOrElse(a.value)
    }
    props.foreach { p => propsDict(p.title) = p.value }

    (attrsDict, propsDict, styles.toSnabbdom)
  }

  private def merge[T](first: js.Dictionary[T], second: js.Dictionary[T]): js.Dictionary[T] = {
    val result = js.Dictionary.empty[T]
    first.foreach { case (key, value) => result(key) = value }
    second.foreach { case (key, value) => result(key) = value }
    result
  }

  def updateDataObject(obj: DataObject): DataObject = {

    val (attrs, props, style) = toSnabbdom

    DataObject(
      attrs = merge(obj.attrs, attrs),
      props = merge(obj.props, props),
      style = merge(obj.style, style),
      on = obj.on, hook = obj.hook, key = obj.key
    )
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

  private def createInsertHook(streams: Streams,
    subscription: SingleAssignCancelable,
    hooks: Seq[InsertHook]
  )(implicit s: Scheduler): Hooks.HookSingleFn = (vproxy: VNodeProxy) => {

    def toProxy(prev: VNodeProxy, state: VNodeState): IO[VNodeProxy] = {

      state.modifiers.toList.sequence.flatMap { modifiers =>
        val newProxy = SeparatedModifiers.from(modifiers).toSnabbdom(prev.sel, Some(prev))
        newProxy.map { proxy =>
          println("--- Prev: ")
          dom.console.log(JSON.stringify(prev))
          println("--- New: ")
          dom.console.log(JSON.stringify(proxy))
          patch(prev, proxy)
        }
      }
    }

    subscription := streams.observable
      .scanEval(IO.pure(vproxy))(toProxy)
      .subscribe(
        _ => Continue,
        error => dom.console.error(error.getMessage + "\n" + error.getStackTrace.mkString("\n"))
      )

    vproxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
  }


  private def createDestroyHook(
    subscription: SingleAssignCancelable, hooks: Seq[DestroyHook]
  ): Hooks.HookSingleFn = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.cancel()
    ()
  }

  def toSnabbdom(streams: Option[Streams])(implicit s: Scheduler): Hooks = {
    val (insertHook, destroyHook) = if (streams.nonEmpty) {
      val subscription = SingleAssignCancelable()
      val insertHook: js.UndefOr[Hooks.HookSingleFn] = createInsertHook(streams.get, subscription, insertHooks)
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

  private def createDataObject()(implicit s: Scheduler): DataObject = {

    val (attrs, props, style) = properties.attributes.toSnabbdom
    val eventHandlers = emitters.toSnabbdom
    val hooks = properties.hooks.toSnabbdom(Option(streams).filter(_ => hasStreams))

    val keyOption = properties.keys.lastOption
    val key = if (hasStreams) {
      keyOption.fold[Key.Value](streams.hashCode)(_.value): js.UndefOr[Key.Value]
    } else {
      keyOption.map(_.value).orUndefined
    }

    DataObject(attrs, props, style, eventHandlers, hooks, key)
  }

  private def updateDataObject(obj: DataObject)(implicit scheduler: Scheduler): DataObject = {
    val (attrs, props, style) = properties.attributes.toSnabbdom
//    val hooks = properties.hooks.toSnabbdom(receivers)
    DataObject(
      attrs = DictionaryOps.merge(obj.attrs, attrs),
      props = DictionaryOps.merge(obj.props, props),
      style = DictionaryOps.merge(obj.style, style),
      on = DictionaryOps.merge(obj.on, emitters.toSnabbdom),
      hook = obj.hook,
//        Hooks(
//        hooks.insert.orElse(obj.hook.insert),
//        hooks.prepatch.orElse(obj.hook.prepatch),
//        hooks.update.orElse(obj.hook.update),
//        hooks.postpatch.orElse(obj.hook.postpatch),
//        hooks.destroy.orElse(obj.hook.destroy)
//      ),
      //TODO: it should not be possible to stream keys!
      key = obj.key
    )
  }

  private[outwatch] def toSnabbdom(
    nodeType: String, previousProxy: Option[VNodeProxy] = None, ensureKey: Boolean = false
  )(implicit scheduler: Scheduler): IO[VNodeProxy] = {

    // if child streams exists, we want the static children in the same node have keys
    // for efficient patching when the streams change
    val childrenWithKey = children.ensureKey(ensureKey || hasStreams)
//    println(childrenWithKey)
    val dataObject = previousProxy.fold(createDataObject())(p => updateDataObject(p.data))

    childrenWithKey match {
      case Children.VNodes(vnodes) =>
        val childProxies = vnodes.collect { case s: StaticVNode => s.toSnabbdom }
        childProxies.sequence.map { proxies =>
          hFunction(nodeType, dataObject, proxies.toJSArray)
        }
      case Children.StringModifiers(textChildren) =>
        IO.pure(hFunction(nodeType, dataObject, textChildren.map(_.string).mkString))
      case Children.Empty =>
        IO.pure(hFunction(nodeType, dataObject))
    }
  }
}
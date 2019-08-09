package outwatch.dom.helpers

import outwatch.dom._

import scala.scalajs.js

// using js.undefined as default value (instead of empty structs) to avoid un-necessary memory allocations
private[outwatch] final class SeparatedModifiers(
  var emitters: js.UndefOr[SeparatedEmitters] = js.undefined,
  var attributes: js.UndefOr[SeparatedAttributes] = js.undefined,
  var hooks: js.UndefOr[SeparatedHooks] = js.undefined,
  var nodes: js.Array[StaticVNode] = js.Array(),
  var hasVtrees: Boolean = false,
  var key: js.UndefOr[Key] = js.undefined
) extends SnabbdomModifiers {

  private def add(m: SimpleModifier): Unit = m match {
    case attr: Attribute =>
      attributes.getOrElse {
        val value = new SeparatedAttributes()
        attributes = value
        value
      }.push(attr)
    case sn: VTree =>
      hasVtrees = true
      nodes.push(sn)
      ()
    case sn: StringVNode =>
      nodes.push(sn)
      ()
    case e: Emitter =>
      emitters.fold {
        emitters = new SeparatedEmitters(js.Array(e))
      } { em => em.push(e); () }
    case SimpleCompositeModifier(mods) =>
      mods.foreach(add)
    case hook: Hook[_] =>
      hooks.getOrElse {
        val value  = new SeparatedHooks()
        hooks = value
        value
      }.push(hook)
    case hook: LifecycleHook =>
      hooks.getOrElse {
        val value = new SeparatedHooks()
        hooks = value
        value
      }.push(hook)
    case EmptyModifier =>
    case k: Key =>
      key = k
  }
}

object SeparatedModifiers {
  def from(mods: js.Array[SimpleModifier]): SeparatedModifiers = {
    val sm = new SeparatedModifiers()
    mods.foreach(sm.add)
    sm
  }
}

private[outwatch] final class SeparatedStyles(
  val styleDict: js.Dictionary[Style.Value] = js.Dictionary[Style.Value](),
  var delayedDict: js.UndefOr[js.Dictionary[String]] = js.undefined,
  var removeDict: js.UndefOr[js.Dictionary[String]] = js.undefined,
  var destroyDict: js.UndefOr[js.Dictionary[String]] = js.undefined
) extends SnabbdomStyles {

  @inline def push(s: Style): Unit = s match {
    case s: BasicStyle =>
      styleDict(s.title) = s.value
    case s: DelayedStyle =>
      delayedDict.fold {
        delayedDict = js.Dictionary[String](s.title -> s.value)
      }(dict => dict(s.title) = s.value)
    case s: RemoveStyle =>
      removeDict.fold {
        removeDict = js.Dictionary[String](s.title -> s.value)
      }(dict => dict(s.title) = s.value)
    case s: DestroyStyle =>
      destroyDict.fold {
        destroyDict = js.Dictionary[String](s.title -> s.value)
      }(dict => dict(s.title) = s.value)
    case a: AccumStyle =>
      styleDict(a.title) = styleDict.get(a.title)
        .fold[Style.Value](a.value)(s => a.accum(s.asInstanceOf[String], a.value))
  }
}


private[outwatch] final class SeparatedAttributes(
  var attrs: js.UndefOr[js.Dictionary[Attr.Value]] = js.undefined,
  var props: js.UndefOr[js.Dictionary[Prop.Value]] = js.undefined,
  var styles: js.UndefOr[SeparatedStyles] = js.undefined
) {
  @inline def push(a: Attribute): Unit = a match {
    case a: BasicAttr =>
      attrs.fold {
        attrs = js.Dictionary[Attr.Value](a.title -> a.value)
      }(dict => dict(a.title) = a.value)
    case a: AccumAttr =>
      attrs.fold {
        attrs = js.Dictionary[Attr.Value](a.title -> a.value)
      }(attrs => attrs(a.title) = attrs.get(a.title).fold(a.value)(a.accum(_, a.value)))
    case p: Prop =>
      props.fold {
        props = js.Dictionary[Prop.Value](p.title -> p.value)
      }(dict => dict(p.title) = p.value)
    case s: Style =>
      styles.fold {
        val stl = new SeparatedStyles()
        stl.push(s)
        styles = stl
      } { stl => stl.push(s); () }
  }
}

private[outwatch] final class SeparatedHooks(
  var insertHooks: js.UndefOr[js.Array[InsertHook]] = js.undefined,
  var prePatchHooks: js.UndefOr[js.Array[PrePatchHook]] = js.undefined,
  var updateHooks: js.UndefOr[js.Array[UpdateHook]] = js.undefined,
  var postPatchHooks: js.UndefOr[js.Array[PostPatchHook]] = js.undefined,
  var destroyHooks: js.UndefOr[js.Array[DestroyHook]] = js.undefined,
  var insertProxyHooks: js.UndefOr[js.Array[InsertLifecycleHook]] = js.undefined,
  var destroyProxyHooks: js.UndefOr[js.Array[DestroyLifecycleHook]] = js.undefined
) extends SnabbdomHooks {
  @inline def push(h: Hook[_]): Unit = h match {
    case h: InsertHook =>
      insertHooks.fold {
        insertHooks = js.Array(h)
      } { hk => hk.push(h); () }
    case h: PrePatchHook =>
      prePatchHooks.fold {
        prePatchHooks = js.Array(h)
      } { hk => hk.push(h); () }
    case h: UpdateHook =>
      updateHooks.fold {
        updateHooks = js.Array(h)
      } { hk => hk.push(h); () }
    case h: PostPatchHook =>
      postPatchHooks.fold {
        postPatchHooks = js.Array(h)
      } { hk => hk.push(h); () }
    case h: DestroyHook =>
      destroyHooks.fold {
        destroyHooks = js.Array(h)
      } { hk => hk.push(h); () }
  }

  @inline def push(h: LifecycleHook): Unit = h match {
    case h: InsertLifecycleHook =>
      insertProxyHooks.fold {
        insertProxyHooks = js.Array(h)
      } { hk => hk.push(h); () }
    case h: DestroyLifecycleHook =>
      destroyProxyHooks.fold {
        destroyProxyHooks = js.Array(h)
      } { hk => hk.push(h); () }

  }
}

private[outwatch] final class SeparatedEmitters(
  val emitters: js.Array[Emitter]
) extends SnabbdomEmitters {
  @inline def push(e: Emitter): Unit = {
    emitters.push(e)
    ()
  }
}


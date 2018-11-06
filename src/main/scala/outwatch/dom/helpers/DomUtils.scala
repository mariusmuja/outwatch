package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom
import outwatch.dom._
import snabbdom.{VNodeProxy, patch}

import scala.scalajs.js

private[outwatch] case class VNodeState(
  initial: SeparatedModifiers,
  stream: Observable[SeparatedModifiers]
) extends SnabbdomState


object VNodeState {

  private type Updater = js.Array[SimpleModifier] => js.Array[SimpleModifier]

  private object KeyGen {
    private var value = KeyGen.##

    def newKey: Key = {
      value += 1
      Key(value)
    }
  }

  private def ensureKeys(mods: js.Array[SimpleModifier]): Unit = {
    var hasKey = false
    mods.indices.foreach { index =>
      mods(index) match {
        case vtree: VTree => mods(index) = vtree.copy(modifiers = KeyGen.newKey +: vtree.modifiers)
        case _: Key => hasKey = true
        case _ =>
      }
    }
    // key must be appended at the end, original positions can be updated by the streams
    if (!hasKey) {
      mods += KeyGen.newKey
      ()
    }
  }

  // separates modifiers into SimpleModifier(s) and ModifierStream(s)
  private def separateStreams(mods: Seq[Modifier]): (js.Array[SimpleModifier], js.Array[(Int, ModifierStream)]) = {

    // flatten first
    val flattened = js.Array[FlatModifier]()
    flattened.sizeHint(mods.length)

    def flattenHelper(mods: Seq[Modifier]): Unit = {
      mods.foreach {
        case CompositeModifier(inner) => flattenHelper(inner)
        case m: FlatModifier => flattened += m
      }
    }
    flattenHelper(mods)

    // separate
    val modifiers = new js.Array[SimpleModifier](flattened.size)
    val streams = js.Array[(Int, ModifierStream)]()

    flattened.indices.foreach { index =>
      flattened(index) match {
        case m: SimpleModifier => modifiers(index) = m
        case m: ModifierStream =>
          modifiers(index) = EmptyModifier
          streams += index -> m
      }
    }

    // ensure key present for VTrees with stream siblings, as well as for the VTree containing the streams
    if (streams.nonEmpty) ensureKeys(modifiers)

    (modifiers, streams)
  }

  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: SimpleModifier => Observable.pure { a => a.update(index, m); a } // (_.updated(index,m))
    case m: CompositeModifier => updaterCM(index, m)
    case m: ModifierStream => updaterMS(index, m)
  }

  private def updaterMS(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater](m => updaterM(index, m) )
  }

  private def updaterCM(index: Int, cm: CompositeModifier): Observable[Updater] = {
    val (modifiers, streams) = separateStreams(cm.modifiers)

    if (streams.nonEmpty) {
      Observable(streams.map { case (idx, ms) => updaterMS(idx, ms) }: _*).merge
        .scan(modifiers)((mods, func) => func(mods))
        .startWith(Seq(modifiers))
        .map(mods => _.updated(index, SimpleCompositeModifier(mods)))
    }
    else Observable.pure(_.updated(index, SimpleCompositeModifier(modifiers)))
  }

  def lifecycleHooks(observable: Observable[SeparatedModifiers]): Seq[LifecycleHook] = {

    var cancelable: Option[Cancelable] = None
    val insertHook = InsertProxyHook{ (vproxy, scheduler) =>
      implicit val s: Scheduler = scheduler

      def patchProxy(prev: VNodeProxy, modifiers: SeparatedModifiers): VNodeProxy = {
        val t1 = System.nanoTime()
        val proxy = modifiers.toSnabbdom(prev.sel)
        val t2 = System.nanoTime()
        dom.console.log("toSnabbdom time: " + (t2 - t1).toDouble/1000000)
        val res = patch(prev, proxy)
        val t3 = System.nanoTime()
        dom.console.log("patch time: " + (t3 - t2).toDouble/1000000)
        res
      }

      if (cancelable.isDefined) {
        dom.console.error("Cancelable subscription already present on insert hook, this is indicative of a bug.")
      }
      cancelable = Some(
        observable.subscribe(
          mods => {
            val newProxy = patchProxy(vproxy, mods)
            vproxy.copyFrom(newProxy)
            Continue
          },
          e => dom.console.error(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
        )
      )
    }

    val destroyHook = DestroyProxyHook { (_, _) =>
      cancelable.foreach(_.cancel())
      cancelable = None
    }

    Seq(insertHook, destroyHook)
  }

  private[outwatch] def from(mods: Seq[Modifier]): VNodeState = {
    val (modifiers, streams) = separateStreams(mods)

    if (streams.isEmpty) {
      VNodeState(SeparatedModifiers.from(modifiers), Observable.empty)
    } else {

      val modifierStream =
        Observable(streams.map { case (index, ms) => updaterMS(index, ms) }: _*).merge
          .scan(modifiers)((mods, func) => func(mods))
          .map(SeparatedModifiers.from)

      // hooks must be appended at the end, original positions can be updated by the streams
      val modifiersWithLifecycle = modifiers ++ lifecycleHooks(modifierStream)

      VNodeState(SeparatedModifiers.from(modifiersWithLifecycle), modifierStream)
    }
  }
}

private[outwatch] final case class SeparatedModifiers(
  emitters: SeparatedEmitters = SeparatedEmitters(),
  attributes: SeparatedAttributes = SeparatedAttributes(),
  hooks: SeparatedHooks = SeparatedHooks(),
  nodes: js.Array[StaticVNode] = js.Array(),
  var hasVtrees: Boolean = false,
  var key: js.UndefOr[Key] = js.undefined
) extends SnabbdomModifiers { self =>

  private def add(m: SimpleModifier): Unit = m match {
    case SimpleCompositeModifier(mods) => mods.foreach(add)
    case EmptyModifier =>
    case e: Emitter => emitters.push(e)
    case attr: Attribute => attributes.push(attr)
    case hook: Hook[_] => hooks.push(hook)
    case hook: LifecycleHook => hooks.push(hook)
    case sn: StringVNode => nodes.push(sn); ()
    case sn: VTree =>
      hasVtrees = true
      nodes.push(sn)
      ()
    case k: Key => key = k
  }
}

object SeparatedModifiers {
  def from(mods: js.Array[SimpleModifier]): SeparatedModifiers = {
    val sm = SeparatedModifiers()
    mods.foreach(sm.add)
    sm
  }
}

private[outwatch] final case class SeparatedStyles(
  styleDict: js.Dictionary[Style.Value] = js.Dictionary[Style.Value](),
  var delayedDict: js.UndefOr[js.Dictionary[String]] = js.undefined,
  var removeDict: js.UndefOr[js.Dictionary[String]] = js.undefined,
  var destroyDict: js.UndefOr[js.Dictionary[String]] = js.undefined
) extends SnabbdomStyles {

  @inline def push(s: Style): Unit = s match {
    case s: BasicStyle => styleDict(s.title) = s.value
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


private[outwatch] final case class SeparatedAttributes(
  attrs: js.Dictionary[Attr.Value] = js.Dictionary[Attr.Value](),
  props: js.Dictionary[Prop.Value] = js.Dictionary[Prop.Value](),
  styles: SeparatedStyles = SeparatedStyles()
) {
  @inline def push(a: Attribute): Unit = a match {
    case a: BasicAttr => attrs(a.title) = a.value
    case a: AccumAttr => attrs(a.title) = attrs.get(a.title).fold(a.value)(a.accum(_, a.value))
    case p: Prop => props(p.title) = p.value
    case s: Style => styles.push(s)
  }
}

private[outwatch] final case class SeparatedHooks(
  var insertHooks: js.UndefOr[js.Array[InsertHook]] = js.undefined,
  var prePatchHooks: js.UndefOr[js.Array[PrePatchHook]] = js.undefined,
  var updateHooks: js.UndefOr[js.Array[UpdateHook]] = js.undefined,
  var postPatchHooks: js.UndefOr[js.Array[PostPatchHook]] = js.undefined,
  var destroyHooks: js.UndefOr[js.Array[DestroyHook]] = js.undefined,
  var insertProxyHooks: js.UndefOr[js.Array[InsertProxyHook]] = js.undefined,
  var destroyProxyHooks: js.UndefOr[js.Array[DestroyProxyHook]] = js.undefined
) extends SnabbdomHooks {
  @inline def push(h: Hook[_]): Unit = h match {
    case h: InsertHook =>
      insertHooks.fold {
        insertHooks = js.Array(h)
      }(_.push(h))
    case h: PrePatchHook =>
      prePatchHooks.fold {
        prePatchHooks = js.Array(h)
      }(_.push(h))
    case h: UpdateHook =>
      updateHooks.fold {
        updateHooks = js.Array(h)
      }(_.push(h))
    case h: PostPatchHook =>
      postPatchHooks.fold {
        postPatchHooks = js.Array(h)
      }(_.push(h))
    case h: DestroyHook =>
      destroyHooks.fold {
        destroyHooks = js.Array(h)
      }(_.push(h))
  }

  @inline def push(h: LifecycleHook): Unit = h match {
    case h: InsertProxyHook =>
      insertProxyHooks.fold {
        insertProxyHooks = js.Array(h)
      }(_.push(h))
    case h: DestroyProxyHook =>
      destroyProxyHooks.fold {
        destroyProxyHooks = js.Array(h)
      }(_.push(h))

  }
}

private[outwatch] final case class SeparatedEmitters(
  var emitters: js.UndefOr[js.Array[Emitter]] = js.undefined
) extends SnabbdomEmitters {
  @inline def push(e: Emitter): Unit =
    emitters.fold {
      emitters = js.Array(e)
    }(_.push(e))
}

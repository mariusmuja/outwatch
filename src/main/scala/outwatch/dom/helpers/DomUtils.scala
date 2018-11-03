package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom
import outwatch.dom._
import snabbdom.{VNodeProxy, patch}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

private[outwatch] case class VNodeState(
  initial: SeparatedModifiers,
  stream: Observable[SeparatedModifiers]
) extends SnabbdomState


object VNodeState {

  private type Updater = Array[SimpleModifier] => Array[SimpleModifier]

  private object KeyGen {
    private var value = KeyGen.##

    def newKey: Key = {
      value += 1
      Key(value)
    }
  }

  private def ensureKeys(mods: ArrayBuffer[SimpleModifier]): Unit = {
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
    }
  }

  // separates modifiers into SimpleModifier(s) and ModifierStream(s)
  private def separateStreams(mods: Seq[Modifier]): (Array[SimpleModifier], Array[(Int, ModifierStream)]) = {

    // flatten first
    val flattened = ArrayBuffer[FlatModifier]()
    flattened.sizeHint(mods.length)

    def flattenHelper(mods: Seq[Modifier]): Unit = {
      mods.foreach {
        case CompositeModifier(inner) => flattenHelper(inner)
        case m: FlatModifier => flattened += m
      }
    }
    flattenHelper(mods)

    // separate
    val modifiers = ArrayBuffer.fill[SimpleModifier](flattened.size)(EmptyModifier)
    val streams = ArrayBuffer.empty[(Int, ModifierStream)]

    flattened.indices.foreach { index =>
      flattened(index) match {
        case m: SimpleModifier => modifiers(index) = m
        case m: ModifierStream => streams += index -> m
      }
    }

    // ensure key present for VTrees with stream siblings, as well as for the VTree containing the streams
    if (streams.nonEmpty) ensureKeys(modifiers)

    (modifiers.toArray, streams.toArray)
  }

  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: SimpleModifier => Observable.pure { a => a.update(index, m); a } // (_.updated(index,m))
    case m: CompositeModifier => updaterCM(index, m)
    case m: ModifierStream => updaterMS(index, m)
  }

  private def updaterMS(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater] { vm =>
      Observable.fromTaskLike(vm).concatMap(m => updaterM(index, m))
    }
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
        val proxy = modifiers.toSnabbdom(prev.sel)
        patch(prev, proxy)
      }

      if (cancelable.isDefined) {
        dom.console.error("Cancelable subscription already present on insert hook, this is indicative of a bug.")
      }
      cancelable = Some(
        observable
        .scan(vproxy)(patchProxy)
        .subscribe(
          newProxy => {
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
  var keyOption: Option[Key] = None
) extends SnabbdomModifiers { self =>

  private def add(m: SimpleModifier): Int = m match {
    case SimpleCompositeModifier(mods) => mods.foreach(add); 0
    case EmptyModifier => 0
    case e: Emitter => emitters.push(e)
    case attr: Attribute => attributes.push(attr); 0
    case hook: Hook[_] => hooks.push(hook)
    case hook: LifecycleHook => hooks.push(hook)
    case sn: StringVNode => nodes.push(sn)
    case sn: VTree =>
      hasVtrees = true
      nodes.push(sn)
    case key: Key => keyOption = Some(key); 0
  }
}

object SeparatedModifiers {
  def from(mods: Array[SimpleModifier]): SeparatedModifiers = {
    val sm = SeparatedModifiers()
    mods.foreach(sm.add)
    sm
  }
}


private[outwatch] final case class SeparatedStyles(
  styleDict: js.Dictionary[Style.Value] = js.Dictionary[Style.Value](),
  delayedDict: js.Dictionary[String] = js.Dictionary[String](),
  removeDict: js.Dictionary[String] = js.Dictionary[String](),
  destroyDict: js.Dictionary[String] = js.Dictionary[String]()
) extends SnabbdomStyles {
  @inline def push(s: Style): Unit = s match {
    case s: BasicStyle => styleDict(s.title) = s.value
    case s: DelayedStyle => delayedDict(s.title) = s.value
    case s: RemoveStyle => removeDict(s.title) = s.value
    case s: DestroyStyle => destroyDict(s.title) = s.value
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
  insertHooks: js.Array[InsertHook] = js.Array(),
  insertProxyHooks: js.Array[InsertProxyHook] = js.Array(),
  prePatchHooks: js.Array[PrePatchHook] = js.Array(),
  updateHooks: js.Array[UpdateHook] = js.Array(),
  postPatchHooks: js.Array[PostPatchHook] = js.Array(),
  destroyHooks: js.Array[DestroyHook] = js.Array(),
  destroyProxyHooks: js.Array[DestroyProxyHook] = js.Array()
) extends SnabbdomHooks {
  @inline def push(h: Hook[_]): Int = h match {
    case ih: InsertHook => insertHooks.push(ih)
    case pph: PrePatchHook => prePatchHooks.push(pph)
    case uh: UpdateHook => updateHooks.push(uh)
    case pph: PostPatchHook => postPatchHooks.push(pph)
    case dh: DestroyHook => destroyHooks.push(dh)
  }

  @inline def push(h: LifecycleHook): Int = h match {
    case h: InsertProxyHook => insertProxyHooks.push(h)
    case h: DestroyProxyHook => destroyProxyHooks.push(h)
  }
}

private[outwatch] final case class SeparatedEmitters(
  emitters: js.Array[Emitter] = js.Array()
) extends SnabbdomEmitters {
  @inline def push(e: Emitter): Int = emitters.push(e)
}

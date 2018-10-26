package outwatch.dom.helpers

import outwatch.dom._

import scala.scalajs.js

private[outwatch] case class VNodeState(
  modifiers: SeparatedModifiers,
  stream: Observable[SeparatedModifiers] = Observable.empty
) extends SnabbdomState

object VNodeState {

  private type Updater = Array[Modifier] => Array[Modifier]

  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: StreamableModifier => Observable.pure(s => s.updated(index, m))
    case m: CompositeModifier => updaterCM(index, m)
    case m: ModifierStream => updaterMS(index, m)
  }

  private def updaterMS(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater] { vm =>
      Observable.fromIO(vm).concatMap(m => updaterM(index, m))
    }
  }

  private def updaterCM(index: Int, cm: CompositeModifier): Observable[Updater] = {

    val modifiers = cm.modifiers.toArray
    val streams = modifiers.zipWithIndex.collect { case (s: ModifierStream, idx) => (s, idx) }

    if (streams.nonEmpty) {
      Observable.merge(streams.map { case (s, idx) => updaterMS(idx, s) }: _*)
        .scan(modifiers)((mods, func) => func(mods))
        .startWith(Seq(modifiers))
        .map(s => (m: Array[Modifier]) => m.updated(index, CompositeModifier(s)))
    }
    else Observable.pure(s => s.updated(index, cm))

  }

  private[outwatch] def from(mods: Array[Modifier]): VNodeState = {

    val streams = mods.zipWithIndex.collect { case (s: ModifierStream, index) => (s, index)}

    val modifiers = if (streams.nonEmpty) {
      mods.map {
        case vtree: VTree => vtree.copy(modifiers = Key(vtree.hashCode) +: vtree.modifiers)
        case m => m
      }
    } else mods

    val modifierStream = if (streams.nonEmpty) {
      Observable.merge(streams.map { case (s, index) => updaterMS(index, s) }: _*)
        .scan(modifiers)((mods, func) => func(mods))
        .map(SeparatedModifiers.from)
    } else Observable.empty

    VNodeState(SeparatedModifiers.from(modifiers), modifierStream)
  }
}

case class Streams(observable: Observable[SeparatedModifiers]) extends AnyVal {
  def nonEmpty: Boolean = observable != Observable.empty
}

object Streams {
  def empty = Streams(Observable.empty)
}

private[outwatch] final case class SeparatedModifiers(
  emitters: SeparatedEmitters = SeparatedEmitters(),
  attributes: SeparatedAttributes = SeparatedAttributes(),
  hooks: SeparatedHooks = SeparatedHooks(),
  children: Children = Children(),
  keys: js.Array[Key] = js.Array()
) extends SnabbdomModifiers { self =>

  private def add(m: Modifier): Unit = {
    m match {
      case _: ModifierStream => 0
      case EmptyModifier => 0
      case e: Emitter => emitters.push(e)
      case cm: CompositeModifier => cm.modifiers.foreach(self.add)
      case attr: Attribute => attributes.push(attr)
      case hook: Hook[_] => hooks.push(hook)
      case sn: StaticVNode => children.push(sn)
      case key: Key => keys.push(key)
    }
  }

}

object SeparatedModifiers {
  def from(mods: Array[Modifier]): SeparatedModifiers = {
    val sm = SeparatedModifiers()
    mods.foreach(sm.add)
    sm
  }
}

private[outwatch] case class Children(
  nodes: js.Array[StaticVNode] = js.Array()
) {

  def push(node: StaticVNode): Int = nodes.push(node)
}

private[outwatch] final case class SeparatedStyles(
  styles: js.Array[Style] = js.Array()
) extends SnabbdomStyles {
  @inline def push(s: Style): Int = styles.push(s)
}


private[outwatch] final case class SeparatedAttributes(
  attrs: js.Array[Attr] = js.Array(),
  props: js.Array[Prop] = js.Array(),
  styles: SeparatedStyles = SeparatedStyles()
) extends SnabbdomAttributes {
  @inline def push(a: Attribute): Int = a match {
    case a : Attr => attrs.push(a)
    case p : Prop => props.push(p)
    case s : Style => styles.push(s)
  }
}

private[outwatch] final case class SeparatedHooks(
  insertHooks: js.Array[InsertHook] = js.Array(),
  prePatchHooks: js.Array[PrePatchHook] = js.Array(),
  updateHooks: js.Array[UpdateHook] = js.Array(),
  postPatchHooks: js.Array[PostPatchHook] = js.Array(),
  destroyHooks: js.Array[DestroyHook] = js.Array()
) extends SnabbdomHooks {
  def push(h: Hook[_]): Int = h match {
    case ih: InsertHook => insertHooks.push(ih)
    case pph: PrePatchHook => prePatchHooks.push(pph)
    case uh: UpdateHook => updateHooks.push(uh)
    case pph: PostPatchHook => postPatchHooks.push(pph)
    case dh: DestroyHook => destroyHooks.push(dh)
  }
}

private[outwatch] final case class SeparatedEmitters(
  emitters: js.Array[Emitter] = js.Array()
) extends SnabbdomEmitters {
  def push(e: Emitter): Int = emitters.push(e)
}

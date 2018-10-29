package outwatch.dom.helpers

import outwatch.dom._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

private[outwatch] case class VNodeState(
  modifiers: SeparatedModifiers,
  stream: Observable[SeparatedModifiers]
) extends SnabbdomState


object VNodeState {

  private type Updater = Array[SimpleModifier] => Array[SimpleModifier]

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
    val simple = ArrayBuffer.fill[SimpleModifier](flattened.size)(EmptyModifier)
    val streams = ArrayBuffer.empty[(Int, ModifierStream)]

    flattened.indices.foreach { index =>
      flattened(index) match {
        case m: SimpleModifier => simple(index) = m
        case m: ModifierStream => streams += index -> m
      }
    }

    // ensure key present for VTrees with stream siblings
    if (streams.nonEmpty) {
      simple.indices.foreach { index =>
        simple(index) match {
          case vtree: VTree =>
            simple(index) = vtree.copy(modifiers = Key(vtree.hashCode) +: vtree.modifiers)
          case _ =>
        }
      }
    }

    (simple.toArray, streams.toArray)
  }

  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: SimpleModifier => Observable.pure(_.updated(index, m))
    case m: CompositeModifier => updaterCM(index, m)
    case m: ModifierStream => updaterMS(index, m)
  }

  private def updaterMS(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater] { vm =>
      Observable.fromIO(vm).concatMap(m => updaterM(index, m))
    }
  }

  private def updaterCM(index: Int, cm: CompositeModifier): Observable[Updater] = {
    val (modifiers, streams) = separateStreams(cm.modifiers)

    if (streams.nonEmpty) {
      Observable.merge(streams.map { case (idx, ms) => updaterMS(idx, ms) }: _*)
        .scan(modifiers)((mods, func) => func(mods))
        .startWith(Seq(modifiers))
        .map(mods => _.updated(index, SimpleCompositeModifier(mods)))
    }
    else Observable.pure(_.updated(index, SimpleCompositeModifier(modifiers)))
  }


  private[outwatch] def from(mods: Array[Modifier]): VNodeState = {
    val (modifiers, streams) = separateStreams(mods)

    val modifierStream = if (streams.nonEmpty) {
      Observable.merge(streams.map { case (index, ms) => updaterMS(index, ms) }: _*)
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
  nodes: js.Array[StaticVNode] = js.Array(),
  keys: js.Array[Key] = js.Array()
) extends SnabbdomModifiers { self =>

  private def add(m: SimpleModifier): Int = m match {
    case SimpleCompositeModifier(mods) => mods.foreach(add); 0
    case EmptyModifier => 0
    case e: Emitter => emitters.push(e)
    case attr: Attribute => attributes.push(attr)
    case hook: Hook[_] => hooks.push(hook)
    case sn: StaticVNode => nodes.push(sn)
    case key: Key => keys.push(key)
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
  @inline def push(h: Hook[_]): Int = h match {
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
  @inline def push(e: Emitter): Int = emitters.push(e)
}

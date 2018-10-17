package outwatch.dom.helpers

import outwatch.dom._

private[outwatch] case class VNodeState(
  modifiers: SeparatedModifiers,
  stream: Observable[SeparatedModifiers] = Observable.empty
) extends SnabbdomState

object VNodeState {

  private type Updater = Array[Modifier] => Array[Modifier]

//
  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: StreamableModifier => Observable.pure{s =>
//      println(s"Update: $index -> $m")
      s.updated(index, m)}
//    case m: CompositeModifier => updater(index, m)
    case m: ModifierStream => updater(index, m)
  }
//
  private def updater(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater] { vm =>
      Observable.fromIO(vm).concatMap(m => updaterM(index, m))
    }
  }
//
//  private def updater(index: Int, cm: CompositeModifier): Observable[Updater] = {
//    Observable.concat(cm.modifiers.reverse.map(m => updaterM(index, m)) : _*)
//  }

//  private def modifierStreamObs(modifiers: Array[Modifier]): Observable[Array[Modifier]] = {
//
//    val streams = modifiers.zipWithIndex.collect { case (s: ModifierStream, index) => (s, index)}
//
//    if (streams.nonEmpty) {
//      Observable.merge(
//        streams.map { case (s, index) => updater(index, s)}: _*
//      ).scan(modifiers)((mods, func) => func(mods))
//    } else Observable.empty
//  }

  private[outwatch] def from(mods: Array[Modifier]): VNodeState = {

    val streams = mods.zipWithIndex.collect { case (s: ModifierStream, index) => (s, index)}

    val modifiers = if (streams.nonEmpty) {
      mods.map {
        case vtree: VTree => vtree.copy(modifiers = Key(vtree.hashCode) +: vtree.modifiers)
        case m => m
      }
    } else mods

    val modifierStream = if (streams.nonEmpty) {
      Observable.merge(
        streams.map { case (s, index) => updater(index, s) }: _*
      )
        .scan(modifiers)((mods, func) => func(mods))
        .map(SeparatedModifiers.from)
    } else Observable.empty

    VNodeState(SeparatedModifiers.from(modifiers), modifierStream)
  }
}

case class Streams(
  observable: Observable[SeparatedModifiers]
) {
  def nonEmpty: Boolean = observable != Observable.empty
}

private[outwatch] final case class SeparatedModifiers(
  emitters: SeparatedEmitters = SeparatedEmitters(),
  attributes: SeparatedAttributes = SeparatedAttributes(),
  hooks: SeparatedHooks = SeparatedHooks(),
  children: Children = Children(),
  keys: List[Key] = Nil
) extends SnabbdomModifiers { self =>

  private def add(index: Int, m: Modifier): SeparatedModifiers = {
    m match {
      case _: ModifierStream => self
      case em: Emitter => copy(emitters = em :: emitters)
      case cm: CompositeModifier => cm.modifiers.foldRight(self)((sm, m) => m.add(index, sm))
      case attr: Attribute => copy(attributes = attr :: attributes)
      case hook: Hook[_] => copy(hooks = hook :: hooks)
      case sn: StaticVNode => copy(children = sn :: children)
      case key: Key => copy(keys = key :: keys)
      case EmptyModifier => self
    }
  }


  def updateFrom(modifiers: Array[Modifier]): SeparatedModifiers = {
    modifiers.zipWithIndex.foldRight(this) { case ((m, index), sm) => sm.add(index, m) }
  }

}

object SeparatedModifiers {
  def from(mods: Array[Modifier]): SeparatedModifiers = SeparatedModifiers().updateFrom(mods)
}

private[outwatch] case class Children(
  nodes: List[StaticVNode] = List.empty) {

  def ::(node: StaticVNode): Children = copy(nodes = node :: nodes)
}

private[outwatch] final case class SeparatedStyles(
  styles: List[Style] = Nil
) extends SnabbdomStyles {
  @inline def ::(s: Style): SeparatedStyles = copy(styles = s :: styles)
}


private[outwatch] final case class SeparatedAttributes(
  attrs: List[Attr] = Nil,
  props: List[Prop] = Nil,
  styles: SeparatedStyles = SeparatedStyles()
) extends SnabbdomAttributes {
  @inline def ::(a: Attribute): SeparatedAttributes = a match {
    case a : Attr => copy(attrs = a :: attrs)
    case p : Prop => copy(props = p :: props)
    case s : Style => copy(styles= s :: styles)
  }
}

private[outwatch] final case class SeparatedHooks(
  insertHooks: List[InsertHook] = Nil,
  prePatchHooks: List[PrePatchHook] = Nil,
  updateHooks: List[UpdateHook] = Nil,
  postPatchHooks: List[PostPatchHook] = Nil,
  destroyHooks: List[DestroyHook] = Nil
) extends SnabbdomHooks {
  def ::(h: Hook[_]): SeparatedHooks = h match {
    case ih: InsertHook => copy(insertHooks = ih :: insertHooks)
    case pph: PrePatchHook => copy(prePatchHooks = pph :: prePatchHooks)
    case uh: UpdateHook => copy(updateHooks = uh :: updateHooks)
    case pph: PostPatchHook => copy(postPatchHooks = pph :: postPatchHooks)
    case dh: DestroyHook => copy(destroyHooks = dh :: destroyHooks)
  }
}

private[outwatch] final case class SeparatedEmitters(
  emitters: List[Emitter] = Nil
) extends SnabbdomEmitters {
  def ::(e: Emitter): SeparatedEmitters = copy(emitters = e :: emitters)
}

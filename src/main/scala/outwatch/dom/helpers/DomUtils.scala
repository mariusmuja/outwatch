package outwatch.dom.helpers

import outwatch.dom._

import scala.collection.breakOut

private[outwatch] case class VNodeState(
  modifiers: SeparatedModifiers,
  stream: Observable[SeparatedModifiers] = Observable.empty
) extends SnabbdomState

object VNodeState {

  private type Updater = SeparatedModifiers => SeparatedModifiers
//
//  private def updaterM(index: Int, m: Modifier): Observable[Updater] = m match {
//    case m: ModifierStream => updater(index, m)
//    case CompositeModifier(modifiers) => modifiers.map(m => updaterM(index, m)).reduce(_ ++ _)
//    case m: StreamableModifier => Observable.pure[Updater](s => s.update(index, m))
//  }

  private def updater(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater](vm =>
      Observable.fromIO(vm).map(m => s => s.update(index, m))
    )
  }

  private def updaters(modStream: Map[Int, ModifierStream]): Seq[Observable[Updater]] =
    modStream.map { case (index, ms) => updater(index, ms) }(breakOut)

  private def modifierStream(separatedModifiers: SeparatedModifiers) : Observable[SeparatedModifiers] = {

    if (separatedModifiers.streams.nonEmpty) {
      Observable.merge(updaters(separatedModifiers.streams): _*)
        .scan(separatedModifiers)((state, updater) => updater(state))
    } else Observable.empty
  }

  private[outwatch] def from(modifiers: Seq[Modifier]): VNodeState = {

    val hasStreams = modifiers.exists {
      case _: ModifierStream => true
      case _ => false
    }

    val modifiersWithKey = if (hasStreams) {
      modifiers.map {
        case vtree: VTree => vtree.copy(modifiers = Key(vtree.hashCode) +: vtree.modifiers)
        case m => m
      }
    } else modifiers

    val mods = SeparatedModifiers(modifiersWithKey.toArray).updateAll()
    VNodeState(mods, modifierStream(mods))
  }
}

case class Streams(
  observable: Observable[SeparatedModifiers]
) {
  def nonEmpty: Boolean = observable != Observable.empty
}

private[outwatch] final case class SeparatedModifiers(
  modifiers: Array[Modifier],
  streams: Map[Int, ModifierStream] = Map.empty,
  emitters: SeparatedEmitters = SeparatedEmitters(),
  attributes: SeparatedAttributes = SeparatedAttributes(),
  hooks: SeparatedHooks = SeparatedHooks(),
  children: Children = Children.Empty,
  keys: List[Key] = Nil
) extends SnabbdomModifiers { self =>

  private def add(index: Int, m: Modifier): SeparatedModifiers = {
    m match {
      case ms: ModifierStream => copy(streams = streams + (index -> ms))
      case em: Emitter => copy(emitters = em :: emitters)
      case cm: CompositeModifier => cm.modifiers.foldRight(self)((sm, m) => m.add(index, sm))
      case attr: Attribute => copy(attributes = attr :: attributes)
      case hook: Hook[_] => copy(hooks = hook :: hooks)
      case sn: StaticVNode => copy(children = sn :: children)
      case sm: StringModifier => copy(children = sm :: children)
      case key: Key => copy(keys = key :: keys)
      case EmptyModifier => self
    }
  }


  def updateAll(): SeparatedModifiers = {
    modifiers.zipWithIndex.foldRight(this) { case ((m, index), sm) => sm.add(index, m) }
  }


  def update(index: Int, m: Modifier): SeparatedModifiers = {

    val filtered = m match {
      case attr: Attribute => modifiers.map {
        case a: Attribute if a.title == attr.title => EmptyModifier
        case m => m
      }
      case _ => modifiers
    }

    val updated = filtered.updated(index, m)

//    println(updated.toList)

    SeparatedModifiers(updated).updateAll()
  }

}

private[outwatch] trait Children {
  def ::(mod: StringModifier): Children

  def ::(node: StaticVNode): Children
}

object Children {
  private def toVNode(mod: StringModifier) = StringVNode(mod.string)
  private def toModifier(node: StringVNode) = StringModifier(node.string)

  private[outwatch] case object Empty extends Children {
    override def ::(mod: StringModifier): Children = StringModifiers(mod :: Nil)

    override def ::(node: StaticVNode): Children = node match {
      case s: StringVNode => toModifier(s) :: this
      case n => n :: VNodes(Nil)
    }
  }

  private[outwatch] case class StringModifiers(modifiers: List[StringModifier]) extends Children {
    override def ::(mod: StringModifier): Children = copy(mod :: modifiers)

    override def ::(node: StaticVNode): Children = node match {
      case s: StringVNode => toModifier(s) :: this // this should never happen
      case n => n :: VNodes(modifiers.map(toVNode))
    }
  }

  private[outwatch] case class VNodes(nodes: List[StaticVNode]) extends Children {
    override def ::(mod: StringModifier): Children = copy(toVNode(mod) :: nodes)

    override def ::(node: StaticVNode): Children = copy(nodes = node :: nodes)
  }
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

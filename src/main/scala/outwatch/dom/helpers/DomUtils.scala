package outwatch.dom.helpers

import cats.effect.IO
import monix.reactive.subjects.BehaviorSubject
import outwatch.dom._

import scala.collection.breakOut


private[outwatch] final case class SeparatedProperties(
  attributes: SeparatedAttributes = SeparatedAttributes(),
  hooks: SeparatedHooks = SeparatedHooks(),
  keys: List[Key] = Nil
) {
  def add(p: Property): SeparatedProperties = p match {
    case att: Attribute => copy(attributes = attributes.add(att))
    case hook: Hook[_] => copy(hooks = hooks.add(hook))
    case key: Key => copy(keys = key :: keys)
  }
}

private[outwatch] final case class SeparatedAttributes(
  attributes: List[Attribute] = Nil
) extends SnabbdomAttributes {
  @inline def add(a: Attribute): SeparatedAttributes = copy(attributes = a :: attributes)
}

private[outwatch] final case class SeparatedHooks(
  insertHooks: List[InsertHook] = Nil,
  prePatchHooks: List[PrePatchHook] = Nil,
  updateHooks: List[UpdateHook] = Nil,
  postPatchHooks: List[PostPatchHook] = Nil,
  destroyHooks: List[DestroyHook] = Nil
) extends SnabbdomHooks {
  def add(h: Hook[_]): SeparatedHooks = h match {
    case ih: InsertHook => copy(insertHooks = ih :: insertHooks)
    case pph: PrePatchHook => copy(prePatchHooks = pph :: prePatchHooks)
    case uh: UpdateHook => copy(updateHooks = uh :: updateHooks)
    case pph: PostPatchHook => copy(postPatchHooks = pph :: postPatchHooks)
    case dh: DestroyHook => copy(destroyHooks = dh :: destroyHooks)
  }
}

private[outwatch] final case class ChildrenNodes(
  allNodes: List[ChildVNode] = Nil,
  staticNodes: List[StaticVNode] = Nil,
  hasStreams: Boolean = false,
  childrenStreams: Int = 0
) {
  // ensure a key is present in the VTree modifiers
  // used to ensure efficient Snabbdom patch operation in the presence of children streams
  private def ensureVTreeKey(vtree: VTree): VTree = {
    val hasKey = vtree.modifiers.exists(m => m.unsafeRunSync().isInstanceOf[Key])
    val newModifiers = if (hasKey) vtree.modifiers else IO.pure(Key(this.hashCode)) +: vtree.modifiers
    vtree.copy(modifiers = newModifiers)
  }

  private def ensureVNodeKey[N >: VTree](node: N): N = node match {
    case vtree: VTree => ensureVTreeKey(vtree)
    case other => other
  }

  def allNodesWithKey: List[ChildVNode] = if (hasStreams) allNodes.map(ensureVNodeKey) else allNodes

  def staticNodesWithKey: List[StaticVNode] = if (hasStreams) staticNodes.map(ensureVNodeKey) else staticNodes

  def add(cn: ChildVNode): ChildrenNodes = cn match {
    case sn: StaticVNode => copy(staticNodes = sn :: staticNodes, allNodes = sn :: allNodes)
    case csr: ChildStreamReceiver => copy(hasStreams = true, allNodes = csr :: allNodes)
    case csr: ChildrenStreamReceiver =>
      copy(hasStreams = true, childrenStreams = childrenStreams + 1, allNodes = csr :: allNodes)
  }
}

private[outwatch] final case class SeparatedEmitters(
  emitters: List[Emitter] = Nil
) extends SnabbdomEmitters {
  def add(e: Emitter): SeparatedEmitters = copy(emitters = e :: emitters)
}

private[outwatch] final case class SeparatedModifiers(
  properties: SeparatedProperties = SeparatedProperties(),
  emitters: SeparatedEmitters = SeparatedEmitters(),
  attributeReceivers: List[AttributeStreamReceiver] = Nil,
  childrenNodes: ChildrenNodes = ChildrenNodes(),
  hasChildVNodes: Boolean = false,
  stringModifiers: List[StringModifier] = Nil
) extends SnabbdomModifiers { self =>

  def add(m: Modifier): SeparatedModifiers = m match {
    case pr: Property => copy(properties = properties.add(pr))
    case vn: ChildVNode => copy(childrenNodes = childrenNodes.add(vn), hasChildVNodes = true)
    case em: Emitter => copy(emitters = emitters.add(em))
    case rc: AttributeStreamReceiver => copy(attributeReceivers = rc :: attributeReceivers)
    case cm: CompositeModifier =>
      val modifiers = cm.modifiers.map(_.unsafeRunSync())
      modifiers.foldRight(self)((m, sm) => sm.add(m))
    case sm: StringModifier =>
      copy(
        // create a String VNode, will only be used if other vnodes also exist
        childrenNodes = childrenNodes.add(StringVNode(sm.string)),
        stringModifiers = sm :: stringModifiers
      )
    case EmptyModifier => self
  }
}

object SeparatedModifiers {
  private[outwatch] def separate(modifiers: Seq[Modifier]): SeparatedModifiers = {
    modifiers.foldRight(SeparatedModifiers())((m, sm) => sm.add(m))
  }
}


private[outwatch] final case class Changeables(
  childNodes: List[ChildVNode],
  hasNodeStreams: Boolean,
  multipleChildrenStreams: Boolean,
  attributeStreamReceivers: List[AttributeStreamReceiver]
) {

  lazy val observable: Observable[(Seq[Attribute], Seq[IO[StaticVNode]])] = {
    val childStreamReceivers = if (hasNodeStreams) {
      childNodes.foldRight(Observable(List.empty[IO[StaticVNode]])) {
        case (vn: StaticVNode, obs) => obs.combineLatestMap(BehaviorSubject(IO.pure(vn)))((nodes, n) => n :: nodes)
        case (csr: ChildStreamReceiver, obs) => obs.combineLatestMap(csr.childStream)((nodes, n) => n :: nodes)
        case (csr: ChildrenStreamReceiver, obs) =>
          obs.combineLatestMap(
            if (multipleChildrenStreams) csr.childrenStream.startWith(Seq(Seq.empty)) else csr.childrenStream
          )((nodes, n) => n.toList ++ nodes)
      }.dropWhile(_.isEmpty)
    } else {
      Observable(Seq.empty)
    }

    // only use last encountered observable per attribute
    val attributeReceivers: Observable[Seq[Attribute]] = Observable.combineLatestList(
      attributeStreamReceivers
        .groupBy(_.attribute)
        .values
        .map(_.last.attributeStream)(breakOut): _*
    )

    attributeReceivers.startWith(Seq(Seq.empty)).combineLatest(
      childStreamReceivers.startWith(Seq(Seq.empty))
    ).dropWhile { case (a, c) => a.isEmpty && c.isEmpty }
  }

  lazy val nonEmpty: Boolean = {
    attributeStreamReceivers.nonEmpty || hasNodeStreams
  }
}
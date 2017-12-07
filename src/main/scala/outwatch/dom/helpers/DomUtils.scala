package outwatch.dom.helpers

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.BehaviorSubject
import org.scalajs.dom
import outwatch.dom._
import snabbdom._

import scala.collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


private[outwatch] final case class SeparatedProperties(
  hooks: SeparatedHooks = SeparatedHooks(),
  attributes: Attributes = Attributes(),
  keys: List[Key] = Nil
) {
  def add(p: Property): SeparatedProperties = p match {
    case att: Attribute => copy(attributes = attributes.add(att))
    case hook: Hook[_] => copy(hooks = hooks.add(hook))
    case key: Key => copy(keys = key :: keys)
  }
}

private[outwatch] final case class Attributes(
  attributes: List[Attribute] = Nil
) { self =>

  @inline def add(a: Attribute): Attributes = copy(attributes = a :: attributes)

  def toSnabbdom: (js.Dictionary[Attr.Value], js.Dictionary[Prop.Value], js.Dictionary[String]) = {
    val attrsDict = js.Dictionary[Attr.Value]()
    val propsDict = js.Dictionary[Prop.Value]()
    val styleDict = js.Dictionary[String]()

    attributes.foreach {
      case a: AccumAttr => attrsDict(a.title) = attrsDict.get(a.title).map(a.accum(_, a.value)).getOrElse(a.value)
      case a: Attr => attrsDict(a.title) = a.value
      case a: Prop => propsDict(a.title) = a.value
      case a: Style => styleDict(a.title) = a.value
      case EmptyAttribute =>
    }

    (attrsDict, propsDict, styleDict)
  }

  def updateAttributes(obj: DataObject): DataObject = {
    import scala.scalajs.js.JSConverters._

    val (attrs, props, style) = toSnabbdom
    val newAttrs = (obj.attrs ++ attrs).toJSDictionary
    val newProps = (obj.props ++ props).toJSDictionary
    val newStyle = (obj.style ++ style).toJSDictionary
    DataObject(attrs = newAttrs, props = newProps, style = newStyle, on = obj.on, hook = obj.hook, key = obj.key)
  }
}

private[outwatch] final case class SeparatedHooks(
  insertHooks: List[InsertHook] = Nil,
  prePatchHooks: List[PrePatchHook] = Nil,
  updateHooks: List[UpdateHook] = Nil,
  postPatchHooks: List[PostPatchHook] = Nil,
  destroyHooks: List[DestroyHook] = Nil
) {
  def add(h: Hook[_]): SeparatedHooks = h match {
    case ih: InsertHook => copy(insertHooks = ih :: insertHooks)
    case pph: PrePatchHook => copy(prePatchHooks = pph :: prePatchHooks)
    case uh: UpdateHook => copy(updateHooks = uh :: updateHooks)
    case pph: PostPatchHook => copy(postPatchHooks = pph :: postPatchHooks)
    case dh: DestroyHook => copy(destroyHooks = dh :: destroyHooks)
  }

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


  private def createInsertHook(changables: Changeables,
    subscriptionRef: STRef[Cancelable],
    hooks: Seq[InsertHook]
  ): Hooks.HookSingleFn = (proxy: VNodeProxy) => {

    def toProxy(changable: (Seq[Attribute], Seq[IO[StaticVNode]])): VNodeProxy = {
      val (attributes, nodes) = changable

      hFunction(
        proxy.sel,
        Attributes(attributes.toList).updateAttributes(proxy.data),
        if (nodes.isEmpty) proxy.children else nodes.map(_.unsafeRunSync().asProxy)(breakOut): js.Array[VNodeProxy]
      )
    }

    val subscription = changables.observable
      .map(toProxy)
      .startWith(Seq(proxy))
      .bufferSliding(2, 1)
      .subscribe(
        { case Seq(old, crt) => patch(old, crt); Continue },
        error => dom.console.error(error.getMessage)
      )

    subscriptionRef.put(subscription).unsafeRunSync()

    proxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
  }


  private def createDestroyHook(
    subscription: STRef[Cancelable],hooks: Seq[DestroyHook]
  ): Hooks.HookSingleFn = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: dom.Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.update { s => s.cancel(); s }.unsafeRunSync()
    ()
  }

  def toSnabbdom(changeables: Changeables): Hooks = {
    val (insertHook, destroyHook) = if (changeables.nonEmpty) {
      val subscriptionRef = STRef.empty[Cancelable]
      val insertHook: js.UndefOr[Hooks.HookSingleFn] = createInsertHook(changeables, subscriptionRef, insertHooks)
      val destroyHook: js.UndefOr[Hooks.HookSingleFn] = createDestroyHook(subscriptionRef, destroyHooks)
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

private[outwatch] final case class ChildrenNodes(
  allNodes: List[ChildVNode] = Nil,
  staticNodes: List[StaticVNode] = Nil,
  hasStreams: Boolean = false,
  childrenStreams: Int = 0
) {
  def add(cn: ChildVNode): ChildrenNodes = cn match {
    case sn: StaticVNode => copy(staticNodes = sn :: staticNodes, allNodes = sn :: allNodes)
    case csr: ChildStreamReceiver => copy(hasStreams = true, allNodes = csr :: allNodes)
    case csr: ChildrenStreamReceiver =>
      copy(hasStreams = true, childrenStreams = childrenStreams + 1, allNodes = csr :: allNodes)
  }
}

final case class Emitters(
  emitters: List[Emitter] = Nil
) {
  def add(e: Emitter): Emitters = copy(emitters = e :: emitters)

  private def emittersToFunction(emitters: Seq[Emitter]): js.Function1[dom.Event, Unit] = {
    (event: dom.Event) => emitters.foreach(_.trigger(event))
  }

  def toSnabbdom: js.Dictionary[js.Function1[dom.Event, Unit]] = {
    emitters
      .groupBy(_.eventType)
      .mapValues(emittersToFunction)
      .toJSDictionary
  }
}

private[outwatch] final case class SeparatedModifiers(
  properties: SeparatedProperties,
  emitters: Emitters,
  attributeReceivers: List[AttributeStreamReceiver],
  childrenNodes: ChildrenNodes,
  hasChildVNodes: Boolean,
  stringModifiers: List[StringModifier]
) {
  self =>

  def add(m: Modifier): SeparatedModifiers = m match {
    case pr: Property => copy(properties = properties.add(pr))
    case vn: ChildVNode => copy(childrenNodes = childrenNodes.add(vn), hasChildVNodes = true)
    case em: Emitter => copy(emitters = emitters.add(em))
    case rc: AttributeStreamReceiver => copy(attributeReceivers = rc :: attributeReceivers)
    case cm: CompositeModifier =>
      val modifiers = cm.modifiers.map(_.unsafeRunSync())
      modifiers.foldRight(self)((m, sm) => sm.add(m))
    case sm: StringModifier =>
      copy(childrenNodes = childrenNodes.add(StringVNode(sm.string)), stringModifiers = sm :: stringModifiers)
    case EmptyModifier => self
  }


  private def createDataObject(changeables: Changeables): DataObject = {

    val keyOption = properties.keys.lastOption
    val key = if (changeables.nonEmpty) {
      keyOption.fold[Key.Value](changeables.hashCode)(_.value): js.UndefOr[Key.Value]
    } else {
      keyOption.map(_.value).orUndefined
    }

    val (attrs, props, style) = properties.attributes.toSnabbdom
    DataObject(
      attrs, props, style, emitters.toSnabbdom,
      properties.hooks.toSnabbdom(changeables),
      key
    )
  }

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

  private[outwatch] def toSnabbdom(nodeType: String): VNodeProxy = {

    val hasChildStreams = childrenNodes.hasStreams

    // if child streams exists, we want the static children in the same node have keys
    // for efficient patching when the streams change
    val staticNodes = if (hasChildStreams) {
      childrenNodes.staticNodes.map(ensureVNodeKey)
    } else childrenNodes.staticNodes

    val nodes = if (hasChildStreams) {
      childrenNodes.allNodes.map(ensureVNodeKey)
    } else childrenNodes.allNodes


    val changeables = Changeables(nodes, hasChildStreams, childrenNodes.childrenStreams > 1, attributeReceivers)
    val dataObject = createDataObject(changeables)

    //TODO: use .sequence instead of unsafeRunSync?
    // import cats.instances.list._
    // import cats.syntax.traverse._
    // for { childProxies <- children.map(_.value).sequence }
    // yield hFunction(nodeType, attributeObject, childProxies.map(_.apsProxy)(breakOut))
    if (hasChildVNodes) { // children.nonEmpty doesn't work, children will always include StringModifiers as StringNodes
      val childProxies: js.Array[VNodeProxy] = staticNodes.map(_.asProxy)(breakOut)
      hFunction(nodeType, dataObject, childProxies)
    }
    else if (stringModifiers.nonEmpty) {
      hFunction(nodeType, dataObject, stringModifiers.map(_.string).mkString)
    }
    else {
      hFunction(nodeType, dataObject)
    }
  }
}

object SeparatedModifiers {
  private[outwatch] def create(args: Seq[Modifier]): SeparatedModifiers = {
    args.foldRight(SeparatedModifiers(
      SeparatedProperties(), Emitters(), Nil, ChildrenNodes(), hasChildVNodes = false, Nil
    )
    )((m, sm) => sm.add(m))
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


object DomUtils {

  def render(element: dom.Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    elem <- IO(dom.document.createElement("app"))
    _ <- IO(element.appendChild(elem))
    _ <- IO(patch(elem, node.asProxy))
  } yield ()
}

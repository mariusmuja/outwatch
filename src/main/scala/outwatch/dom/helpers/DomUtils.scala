package outwatch.dom.helpers

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.BehaviorSubject
import org.scalajs.dom._
import outwatch.dom._
import snabbdom._

import scala.collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.JSConverters._


object DomUtils {

  private def createDataObject(changeables: SeparatedReceivers,
                               properties: Seq[Property],
                               eventHandlers: js.Dictionary[js.Function1[Event, Unit]]): DataObject = {

    if (changeables.nonEmpty){
      createReceiverDataObject(changeables, properties, eventHandlers)
    } else {
      createSimpleDataObject(properties, eventHandlers)
    }
  }

  private def createSimpleDataObject(properties: Seq[Property], handlers: js.Dictionary[js.Function1[Event, Unit]]) = {

    val SeparatedProperties(insert, prepatch, update, postpatch, destroy, attributes, keys) = separateProperties(properties)
    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)

    val insertHook = createHookSingle(insert)
    val prePatchHook = createHookPairOption(prepatch)
    val updateHook = createHookPair(update)
    val postPatchHook = createHookPair(postpatch)
    val destroyHook = createHookSingle(destroy)
    val key = keys.lastOption.map(_.value).orUndefined

    DataObject(attrs, props, style, handlers,
      Hooks(insertHook, prePatchHook, updateHook, postPatchHook, destroyHook),
      key
    )
  }

  private def createReceiverDataObject(changeables: SeparatedReceivers,
                                       properties: Seq[Property],
                                       eventHandlers: js.Dictionary[js.Function1[Event, Unit]]) = {

    val SeparatedProperties(insert, prepatch, update, postpatch, destroy, attributes, keys) = separateProperties(properties)

    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)
    val subscriptionRef = STRef.empty[Cancelable]

    val insertHook = createInsertHook(changeables, subscriptionRef, insert)
    val prePatchHook = createHookPairOption(prepatch)
    val updateHook = createHookPair(update)
    val postPatchHook = createHookPair(postpatch)
    val destroyHook = createDestroyHook(subscriptionRef, destroy)
    val key = keys.lastOption.fold[Key.Value](changeables.hashCode)(_.value)

    DataObject(
      attrs, props, style, eventHandlers,
      Hooks(insertHook, prePatchHook, updateHook, postPatchHook, destroyHook),
      key
    )
  }

  private def createHookSingle(hooks: Seq[Hook[Element]]): js.UndefOr[Hooks.HookSingleFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookSingleFn](hooks =>
      (p: VNodeProxy) => for (e <- p.elm) hooks.foreach(_.observer.onNext(e))
    ).orUndefined
  }

  private def createHookPair(hooks: Seq[Hook[(Element, Element)]]): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => for (o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.onNext((o, c)))
    ).orUndefined
  }

  private def createHookPairOption(hooks: Seq[Hook[(Option[Element], Option[Element])]]): js.UndefOr[Hooks.HookPairFn] = {
    Option(hooks).filter(_.nonEmpty).map[Hooks.HookPairFn](hooks =>
      (old: VNodeProxy, cur: VNodeProxy) => hooks.foreach(_.observer.onNext((old.elm.toOption, cur.elm.toOption)))
    ).orUndefined
  }

  private def createInsertHook(changables: SeparatedReceivers,
                               subscriptionRef: STRef[Cancelable],
                               hooks: Seq[InsertHook]): Hooks.HookSingleFn = (proxy: VNodeProxy) => {

    def toProxy(changable: (Seq[Attribute], Seq[IO[StaticVNode]])): VNodeProxy = {
      val (attributes, nodes) = changable

      hFunction(
        proxy.sel,
        proxy.data.withUpdatedAttributes(attributes),
        if (nodes.isEmpty) proxy.children else nodes.map(_.unsafeRunSync().asProxy)(breakOut): js.Array[VNodeProxy]
      )
    }

    val subscription = changables.observable
      .map(toProxy)
      .startWith(Seq(proxy))
      .bufferSliding(2, 1)
      .subscribe(
        { case Seq(old, crt) => patch(old, crt); Continue },
        error => console.error(error.getMessage)
      )

    subscriptionRef.put(subscription).unsafeRunSync()

    proxy.elm.foreach((e: Element) => hooks.foreach(_.observer.onNext(e)))
  }



  private def createDestroyHook(subscription: STRef[Cancelable], hooks: Seq[DestroyHook]): Hooks.HookSingleFn = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.update { s => s.cancel(); s }.unsafeRunSync()
    ()
  }


  private[outwatch] final case class SeparatedModifiers(
    emitters: List[Emitter] = Nil,
    attributeReceivers: List[AttributeStreamReceiver] = Nil,
    properties: List[Property] = Nil,
    vNodes: List[ChildVNode] = Nil,
    hasChildVNodes : Boolean = false,
    stringModifiers: List[StringModifier] = Nil
  )
  private[outwatch] def separateModifiers(args: Seq[Modifier]): SeparatedModifiers = {
    args.foldRight(SeparatedModifiers())(separatorFn)
  }

  private[outwatch] def separatorFn(mod: Modifier, res: SeparatedModifiers): SeparatedModifiers = (mod, res) match {
    case (em: Emitter, sf) => sf.copy(emitters = em :: sf.emitters)
    case (rc: AttributeStreamReceiver, sf) => sf.copy(attributeReceivers = rc :: sf.attributeReceivers)
    case (pr: Property, sf) => sf.copy(properties = pr :: sf.properties)
    case (vn: ChildVNode, sf) =>
      sf.copy(vNodes = vn :: sf.vNodes, hasChildVNodes = true)
    case (sm: StringModifier, sf) =>
      sf.copy(vNodes = StringVNode(sm.string) :: sf.vNodes, stringModifiers = sm :: sf.stringModifiers)
    case (vn: CompositeModifier, sf) =>
      val modifiers = vn.modifiers.map(_.unsafeRunSync())
      val sm = separateModifiers(modifiers)
      SeparatedModifiers(
        emitters = sm.emitters ++ sf.emitters,
        attributeReceivers = sm.attributeReceivers ++ sf.attributeReceivers,
        properties = sm.properties ++ sf.properties,
        vNodes = sm.vNodes ++ sf.vNodes,
        hasChildVNodes = sm.hasChildVNodes || sf.hasChildVNodes,
        stringModifiers = sm.stringModifiers ++ sf.stringModifiers
      )
    case (EmptyModifier, sf) => sf
  }

  private[outwatch] final case class SeparatedReceivers(
    childNodes: List[ChildVNode] = Nil,
    hasNodeStreams: Boolean = false,
    multipleChildrenStreams: Boolean = false,
    attributeStreamReceivers: List[AttributeStreamReceiver] = Nil
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
          .map(_.last.attributeStream)(breakOut) : _*
      )

      attributeReceivers.startWith(Seq(Seq.empty)).combineLatest(
        childStreamReceivers.startWith(Seq(Seq.empty))
      ).dropWhile { case (a, c) => a.isEmpty && c.isEmpty }
    }

    lazy val nonEmpty: Boolean = {
      attributeStreamReceivers.nonEmpty || hasNodeStreams
    }
  }

  private[outwatch] final case class SeparatedProperties(
    insertHooks: List[InsertHook] = Nil,
    prePatchHooks: List[PrePatchHook] = Nil,
    updateHooks: List[UpdateHook] = Nil,
    postPatchHooks: List[PostPatchHook] = Nil,
    destroyHooks: List[DestroyHook] = Nil,
    attributeHooks: List[Attribute] = Nil,
    keys: List[Key] = Nil
  )
  private[outwatch] def separateProperties(properties: Seq[Property]): SeparatedProperties = {
    properties.foldRight(SeparatedProperties()) {
      case (ih: InsertHook, sp) => sp.copy(insertHooks = ih :: sp.insertHooks)
      case (pph: PrePatchHook, sp) => sp.copy(prePatchHooks = pph :: sp.prePatchHooks)
      case (uh: UpdateHook, sp) => sp.copy(updateHooks = uh :: sp.updateHooks)
      case (pph: PostPatchHook, sp) => sp.copy(postPatchHooks = pph :: sp.postPatchHooks)
      case (dh: DestroyHook, sp) => sp.copy(destroyHooks = dh :: sp.destroyHooks)
      case (at: Attribute, sp) => sp.copy(attributeHooks = at :: sp.attributeHooks)
      case (key: Key, sp) => sp.copy(keys = key :: sp.keys)
    }
  }


  private final case class ChildrenNodes(
    children: List[StaticVNode] = Nil,
    hasStreams: Boolean = false,
    childrenStreams: Int = 0
  )
  private def extractChildren(nodes: Seq[ChildVNode]): ChildrenNodes = nodes.foldRight(ChildrenNodes()) {
    case (vn: StaticVNode, cn) => cn.copy(children = vn :: cn.children)
    case (_: ChildStreamReceiver, cn) => cn.copy(hasStreams = true)
    case (_: ChildrenStreamReceiver, cn) => cn.copy(hasStreams = true, childrenStreams = cn.childrenStreams + 1)
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

  private[outwatch] def extractChildrenAndDataObject(args: Seq[Modifier]): (Seq[StaticVNode], DataObject, Boolean, Seq[StringModifier]) = {
    val SeparatedModifiers(emitters, attributeReceivers, properties, nodes, hasChildVNodes, stringModifiers) = separateModifiers(args)

    val ChildrenNodes(children, hasChildStreams, childrenStreams) = extractChildren(nodes)

    // if child streams exists, we want the static children in the same node have keys
    // for efficient patching when the streams change
    val childrenWithKey = if (hasChildStreams) children.map(ensureVNodeKey) else children
    val nodesWithKey = if (hasChildStreams) nodes.map(ensureVNodeKey) else nodes

    val changeables = SeparatedReceivers(nodesWithKey, hasChildStreams, childrenStreams > 1, attributeReceivers)

    val eventHandlers = VDomProxy.emittersToSnabbDom(emitters)

    val dataObject = createDataObject(changeables, properties, eventHandlers)

    (childrenWithKey, dataObject, hasChildVNodes, stringModifiers)
  }

  def render(element: Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    elem <- IO(document.createElement("app"))
    _ <- IO(element.appendChild(elem))
    _ <- IO(patch(elem, node.asProxy))
  } yield ()
}

package outwatch.dom.helpers

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import org.scalajs.dom._
import outwatch.dom._
import snabbdom._

import collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.BehaviorSubject

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

    val SeparatedProperties(insert, destroy, update, postpatch, attributes, keys) = separateProperties(properties)
    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)

    val insertHook = createInsertHook(insert)
    val updateHook = createUpdateHook(update)
    val postpatchHook = createPostpatchHook(postpatch)
    val destroyHook = createDestroyHook(destroy)

    val key = keys.lastOption.map(_.value).orUndefined

    DataObject.create(attrs, props, style, handlers, insertHook, destroyHook, updateHook, postpatchHook, key)
  }

  private def createReceiverDataObject(changeables: SeparatedReceivers,
                                       properties: Seq[Property],
                                       eventHandlers: js.Dictionary[js.Function1[Event, Unit]]) = {

    val SeparatedProperties(insert, destroy, update, postpatch, attributes, keys) = separateProperties(properties)
    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)
    val subscriptionRef = STRef.empty[Cancelable]

    val insertHook = createInsertHook(changeables, subscriptionRef, insert)
    val updateHook = createUpdateHook(update)
    val postpatchHook = createPostpatchHook(postpatch)
    val destroyHook = createDestroyHook(subscriptionRef, destroy)
    val key = keys.lastOption.fold[Key.Value](changeables.hashCode)(_.value)

    DataObject.create(attrs, props, style, eventHandlers, insertHook, destroyHook, updateHook, postpatchHook, key)
  }

  /* Hooks */

  @inline private def createInsertHook(hooks: Seq[InsertHook]) = (p: VNodeProxy) => {
    p.elm.foreach(e => hooks.foreach(_.observer.onNext(e)))
  }

  private def createInsertHook(changables: SeparatedReceivers,
                               subscriptionRef: STRef[Cancelable],
                               hooks: Seq[InsertHook]) = (proxy: VNodeProxy) => {

    def toProxy(changable: (Seq[Attribute], Seq[VNode])): VNodeProxy = {
      val (attributes, nodes) = changable
      h(
        proxy.sel,
        proxy.data.withUpdatedAttributes(attributes),
        if (nodes.isEmpty) proxy.children else nodes.map(_.unsafeRunSync().asProxy)(breakOut): js.Array[VNodeProxy]
      )
    }

    val subscription = changables.observable
      .map(toProxy)
      .startWith(Seq(proxy))
      .bufferSliding(2, 1)
      .subscribe({ pair =>
        patch(pair.head, pair.tail.head)
        Continue
      }, e => console.error(e.getMessage)
      )

    subscriptionRef.put(subscription).unsafeRunSync()

    proxy.elm.foreach((e: Element) => hooks.foreach(_.observer.onNext(e)))
  }

  @inline private def createUpdateHook(hooks: Seq[UpdateHook]) = (old: VNodeProxy, cur: VNodeProxy) => {
    for(o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.onNext((o,c)))
  }

  @inline private def createPostpatchHook(hooks: Seq[PostpatchHook]) = (old: VNodeProxy, cur: VNodeProxy) => {
    for (o <- old.elm; c <- cur.elm) hooks.foreach(_.observer.onNext((o, c)))
  }

  private def createDestroyHook(subscription: STRef[Cancelable], hooks: Seq[DestroyHook]) = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.update { s => s.cancel(); s }.unsafeRunSync()
    ()
  }

  @inline private def createDestroyHook(hooks: Seq[DestroyHook]) = (p: VNodeProxy) => {
    p.elm.foreach(e => hooks.foreach(_.observer.onNext(e)))
  }


  private[outwatch] final case class SeparatedModifiers(
    emitters: List[Emitter] = Nil,
    attributeReceivers: List[AttributeStreamReceiver] = Nil,
    properties: List[Property] = Nil,
    vNodes: List[ChildVNode] = Nil
  )
  private[outwatch] def separateModifiers(args: Seq[VDomModifier_]): SeparatedModifiers = {
    args.foldRight(SeparatedModifiers())(separatorFn)
  }

  private[outwatch] def separatorFn(mod: VDomModifier_, res: SeparatedModifiers): SeparatedModifiers = (mod, res) match {
    case (em: Emitter, sf) => sf.copy(emitters = em :: sf.emitters)
    case (rc: AttributeStreamReceiver, sf) => sf.copy(attributeReceivers = rc :: sf.attributeReceivers)
    case (pr: Property, sf) => sf.copy(properties = pr :: sf.properties)
    case (vn: ChildVNode, sf) => sf.copy(vNodes = vn :: sf.vNodes)
    case (vn: CompositeVDomModifier, sf) =>
      val modifiers = vn.modifiers.map(_.unsafeRunSync())
      val sm = separateModifiers(modifiers)
      sf.copy(
        emitters = sm.emitters ++ sf.emitters,
        attributeReceivers = sm.attributeReceivers ++ sf.attributeReceivers,
        properties = sm.properties ++ sf.properties,
        vNodes = sm.vNodes ++ sf.vNodes
      )
    case (EmptyVDomModifier, sf) => sf
  }

  private[outwatch] final case class SeparatedReceivers(
    childNodes: List[ChildVNode] = Nil,
    hasNodeStreams: Boolean = false,
    attributeStreamReceivers: List[AttributeStreamReceiver] = Nil
  ) {

    lazy val observable: Observable[(Seq[Attribute], Seq[VNode])] = {
      val childStreamReceivers = if (hasNodeStreams) {
        childNodes.foldRight(Observable(List.empty[VNode])) {
          case (vn: VNode_, obs) => obs.combineLatestMap(BehaviorSubject(IO.pure(vn)))((nodes, n) => n :: nodes)
          case (csr: ChildStreamReceiver, obs) => obs.combineLatestMap(csr.childStream)((nodes, n) => n :: nodes)
          case (csr: ChildrenStreamReceiver, obs) =>
            obs.combineLatestMap(csr.childrenStream.startWith(Seq(Seq.empty)))((nodes, n) => n.toList ++ nodes)
        }
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
    destroyHooks: List[DestroyHook] = Nil,
    updateHooks: List[UpdateHook] = Nil,
    postpatchHooks: List[PostpatchHook] = Nil,
    attributeHooks: List[Attribute] = Nil,
    keys: List[Key] = Nil
  )
  private[outwatch] def separateProperties(properties: Seq[Property]): SeparatedProperties = {
    properties.foldRight(SeparatedProperties()) {
      case (ih: InsertHook, sp) => sp.copy(insertHooks = ih :: sp.insertHooks)
      case (dh: DestroyHook, sp) => sp.copy(destroyHooks = dh :: sp.destroyHooks)
      case (uh: UpdateHook, sp) => sp.copy(updateHooks = uh :: sp.updateHooks)
      case (pph: PostpatchHook, sp) => sp.copy(postpatchHooks = pph :: sp.postpatchHooks)
      case (at: Attribute, sp)  => sp.copy(attributeHooks = at :: sp.attributeHooks)
      case (key: Key, sp) => sp.copy(keys = key :: sp.keys)
    }
  }


  private final case class ChildrenNodes(
    children: List[VNode_] = Nil,
    hasStreams: Boolean = false
  )
  private def extractChildren(nodes: Seq[ChildVNode]): ChildrenNodes = nodes.foldRight(ChildrenNodes()) {
    case (vn: VNode_, cn) => cn.copy(children = vn :: cn.children)
    case (_: ChildStreamReceiver, cn) => cn.copy(hasStreams = true)
    case (_: ChildrenStreamReceiver, cn) => cn.copy(hasStreams = true)
  }

  private[outwatch] def ensureVNodeKey(vn: VNode_): VNode_ = {
    val withKey: VNode_ = vn match {
      case vtree: VTree =>
        val modifiers = vtree.modifiers
        val hasKey = modifiers.exists(m => m.unsafeRunSync().isInstanceOf[Key])
        val newModifiers = if (hasKey) modifiers else IO.pure(Key(vtree.hashCode)) +: modifiers
        vtree.copy(modifiers = newModifiers)
      case sn: StringNode => sn
    }
    withKey
  }

  private[outwatch] def extractChildrenAndDataObject(args: Seq[VDomModifier_]): (Seq[VNode_], DataObject) = {
    val SeparatedModifiers(emitters, attributeReceivers, properties, nodes) = separateModifiers(args)

    val ChildrenNodes(children, hasChildStreams) = extractChildren(nodes)

    // if child streams exists, we want the static children in the same node have keys
    // for efficient patching when the streams change
    val childrenWithKey = if (hasChildStreams) children.map(ensureVNodeKey) else children

    val changeables = SeparatedReceivers(nodes, hasChildStreams, attributeReceivers)

    val eventHandlers = VDomProxy.emittersToSnabbDom(emitters)

    val dataObject = createDataObject(changeables, properties, eventHandlers)

    (childrenWithKey, dataObject)
  }

  def render(element: Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    elem <- IO(document.createElement("app"))
    _ <- IO(element.appendChild(elem))
    _ <- IO(patch(elem, node.asProxy))
  } yield ()
}

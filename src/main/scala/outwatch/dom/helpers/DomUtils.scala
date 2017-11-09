package outwatch.dom.helpers

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLInputElement
import outwatch.dom._
import snabbdom._

import collection.breakOut
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import monix.execution.Scheduler.Implicits.global

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

    val SeparatedProperties(insert, delete, update, attributes, keys) = separateProperties(properties)
    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)

    val insertHook = (p: VNodeProxy) => p.elm.foreach(e => insert.foreach(_.observer.onNext(e)))
    val deleteHook = (p: VNodeProxy) => p.elm.foreach(e => delete.foreach(_.observer.onNext(e)))
    val updateHook = createUpdateHook(update)
    val key = keys.lastOption.map(_.value).orUndefined

    DataObject.create(attrs, props, style, handlers, insertHook, deleteHook, updateHook, key)
  }

  private def seq[A, B](f1: (A, B) => Unit, f2: (A, B) => Unit): (A, B) => Unit = (a: A, b: B) => {
    f1(a, b)
    f2(a, b)
  }

  private val valueSyncHook: (VNodeProxy, VNodeProxy) => Unit = (_, node) => {
    node.elm.foreach { elm =>
      val input = elm.asInstanceOf[HTMLInputElement]
      if (input.value != input.getAttribute("value")) {
        input.value = input.getAttribute("value")
      }
    }
  }

  private def createReceiverDataObject(changeables: SeparatedReceivers,
                                       properties: Seq[Property],
                                       eventHandlers: js.Dictionary[js.Function1[Event, Unit]]) = {

    val SeparatedProperties(insert, destroy, update, attributes, keys) = separateProperties(properties)

    val (attrs, props, style) = VDomProxy.attrsToSnabbDom(attributes)
    val subscriptionRef = STRef.empty[Cancelable]
    val insertHook = createInsertHook(changeables, subscriptionRef, insert)
    val deleteHook = createDestroyHook(subscriptionRef, destroy)
    val updateHook = createUpdateHook(update)
    val key = keys.lastOption.map(_.value).getOrElse(changeables.hashCode.toString)

    val updateHookHelper = if (changeables.valueStreamExists) {
      seq(updateHook, valueSyncHook)
    } else {
      updateHook
    }

    DataObject.create(attrs, props, style, eventHandlers, insertHook, deleteHook, updateHookHelper, key)
  }

  private def createUpdateHook(hooks: Seq[UpdateHook]) = (old: VNodeProxy, cur: VNodeProxy) => {
    for {
      o <- old.elm
      c <- cur.elm
    } {
      hooks.foreach(_.observer.onNext((o,c)))
    }
  }


  private def createInsertHook(changables: SeparatedReceivers,
                               subscriptionRef: STRef[Cancelable],
                               hooks: Seq[InsertHook]) = (proxy: VNodeProxy) => {

    def toProxy(changable: (Seq[Attribute], Seq[VNode])): VNodeProxy = changable match {
      case (attributes, nodes) =>
        val updatedObj = proxy.data.withUpdatedAttributes(attributes)
        h(proxy.sel, updatedObj, proxy.children ++ (nodes.map(_.unsafeRunSync().asProxy)(breakOut): js.Array[VNodeProxy]))
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

  private def createDestroyHook(subscription: STRef[Cancelable], hooks: Seq[DestroyHook]) = (proxy: VNodeProxy) => {
    proxy.elm.foreach((e: Element) => hooks.foreach(_.observer.onNext(e)))
    subscription.update { s => s.cancel(); s }.unsafeRunSync()
    ()
  }


  private[outwatch] final case class SeparatedModifiers(
    emitters: List[Emitter] = Nil,
    receivers: List[Receiver] = Nil,
    properties: List[Property] = Nil,
    vNodes: List[VNode_] = Nil
  )
  private[outwatch] def separateModifiers(args: Seq[VDomModifier_]): SeparatedModifiers = {
    args.foldRight(SeparatedModifiers())(separatorFn)
  }

  private[outwatch] def separatorFn(mod: VDomModifier_, res: SeparatedModifiers): SeparatedModifiers = (mod, res) match {
    case (em: Emitter, sf) => sf.copy(emitters = em :: sf.emitters)
    case (rc: Receiver, sf) => sf.copy(receivers = rc :: sf.receivers)
    case (pr: Property, sf) => sf.copy(properties = pr :: sf.properties)
    case (vn: VNode_, sf) => sf.copy(vNodes = vn :: sf.vNodes)
    case (EmptyVDomModifier, sf) => sf
  }

  private[outwatch] final case class SeparatedReceivers(
    childStreamReceivers: List[ChildStreamReceiver] = Nil,
    childrenStreamReceivers: List[ChildrenStreamReceiver] = Nil,
    attributeStreamReceivers: List[AttributeStreamReceiver] = Nil
  ) {
    lazy val observable: Observable[(Seq[Attribute], Seq[VNode])] = {
      val childReceivers: Observable[Seq[VNode]] = Observable.combineLatestList(
        childStreamReceivers.map(_.childStream) : _*
      )

      val childrenReceivers = childrenStreamReceivers.lastOption.map(_.childrenStream)

      // only use last encountered observable per attribute
      val attributeReceivers: Observable[Seq[Attribute]] = Observable.combineLatestList(
        attributeStreamReceivers
          .groupBy(_.attribute)
          .values
          .map(_.last.attributeStream)(breakOut) : _*
      )

      val allChildReceivers = childrenReceivers.getOrElse(childReceivers)

      attributeReceivers.startWith(Seq(Seq())).combineLatest(
        allChildReceivers.startWith(Seq(Seq()))
      ).dropWhile { case (a, c) => a.isEmpty && c.isEmpty }
    }

    lazy val nonEmpty: Boolean = {
      attributeStreamReceivers.nonEmpty || childrenStreamReceivers.nonEmpty || childStreamReceivers.nonEmpty
    }

    lazy val valueStreamExists: Boolean = attributeStreamReceivers.exists(_.attribute == "value")
  }

  private[outwatch] def separateReceivers(receivers: Seq[Receiver]): SeparatedReceivers = {
    receivers.foldRight(SeparatedReceivers()) {
      case (cr: ChildStreamReceiver, sr) => sr.copy(childStreamReceivers = cr :: sr.childStreamReceivers)
      case (cs: ChildrenStreamReceiver, sr) => sr.copy(childrenStreamReceivers = cs :: sr.childrenStreamReceivers)
      case (ar: AttributeStreamReceiver, sr) => sr.copy(attributeStreamReceivers = ar :: sr.attributeStreamReceivers)
    }
  }

  private[outwatch] final case class SeparatedProperties(
    insertHooks: List[InsertHook] = Nil,
    destroyHooks: List[DestroyHook] = Nil,
    updateHooks: List[UpdateHook] = Nil,
    attributeHooks: List[Attribute] = Nil,
    keys: List[Key] = Nil
  )
  private[outwatch] def separateProperties(properties: Seq[Property]): SeparatedProperties = {
    properties.foldRight(SeparatedProperties()) {
      case (ih: InsertHook, sp) => sp.copy(insertHooks = ih :: sp.insertHooks)
      case (dh: DestroyHook, sp) => sp.copy(destroyHooks = dh :: sp.destroyHooks)
      case (uh: UpdateHook, sp) => sp.copy(updateHooks = uh :: sp.updateHooks)
      case (at: Attribute, sp)  => sp.copy(attributeHooks = at :: sp.attributeHooks)
      case (key: Key, sp) => sp.copy(keys = key :: sp.keys)
    }
  }

  private[outwatch] def extractChildrenAndDataObject(args: Seq[VDomModifier_]): (Seq[VNode_], DataObject) = {
    val SeparatedModifiers(emitters, receivers, properties, children) = separateModifiers(args)

    val changeables = separateReceivers(receivers)

    val eventHandlers = VDomProxy.emittersToSnabbDom(emitters)

    val dataObject = createDataObject(changeables, properties, eventHandlers)

    (children, dataObject)
  }

  def render(element: Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    elem <- IO(document.createElement("app"))
    _ <- IO(element.appendChild(elem))
    _ <- IO(patch(elem, node.asProxy))
  } yield ()
}

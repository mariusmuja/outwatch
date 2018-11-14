package outwatch.dom

import org.scalajs.dom
import org.scalajs.dom._
import snabbdom.patch

object OutWatch {

  def renderInto(element: dom.Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    scheduler <- IO.deferAction(IO.pure)
  } yield {
    val proxy = node.toSnabbdom(scheduler)
    val elem = dom.document.createElement("app")
    element.appendChild(elem)
    patch(elem, proxy)
    ()
  }

  def renderReplace(element: dom.Element, vNode: VNode): IO[Unit] = for {
    node <- vNode
    scheduler <- IO.deferAction(IO.pure)
  } yield {
    val proxy = node.toSnabbdom(scheduler)
    patch(element, proxy)
    ()
  }

  def renderInto(querySelector: String, vNode: VNode): IO[Unit] =
    renderInto(document.querySelector(querySelector), vNode)

  def renderReplace(querySelector: String, vNode: VNode): IO[Unit] =
    renderReplace(document.querySelector(querySelector), vNode)
}

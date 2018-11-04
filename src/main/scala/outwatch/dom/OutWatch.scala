package outwatch.dom

import monix.execution.Scheduler
import org.scalajs.dom
import org.scalajs.dom._
import outwatch.util.Store
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

  @deprecated("Use renderInto instead (or renderReplace)", "0.11.0")
  def render(querySelector: String, vNode: VNode): IO[Unit] = renderInto(querySelector, vNode)

  def renderWithStore[S, A](
    initialState: S, reducer: Store.Reducer[S, A], querySelector: String, root: VNode
  )(implicit s: Scheduler): IO[Unit] =
    Store.renderWithStore(initialState, reducer, querySelector, root)
}

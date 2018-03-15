package outwatch.dom

import cats.effect.{IO, Sync}
import monix.execution.Scheduler
import org.scalajs.dom
import org.scalajs.dom._
import outwatch.util.Store
import snabbdom.patch

object OutWatch {

  def renderInto[F[_]: Sync](element: dom.Element, vNode: VNode)(implicit s: Scheduler): F[Unit] = for {
    node <- vNode
    _ <- Sync[F].delay {
      val elem = dom.document.createElement("app")
      element.appendChild(elem)
      patch(elem, node.toSnabbdom)
    }
  } yield ()

  def renderReplace[F[_]: Sync](element: dom.Element, vNode: VNode)(implicit s: Scheduler): F[Unit] = for {
    node <- vNode
    _ <- Sync[F].delay(patch(element, node.toSnabbdom))
  } yield ()

  def renderInto[F[_]: Sync](querySelector: String, vNode: VNode)(implicit s: Scheduler): F[Unit] =
    renderInto(document.querySelector(querySelector), vNode)

  def renderReplace[F[_]: Sync](querySelector: String, vNode: VNode)(implicit s: Scheduler): F[Unit] =
    renderReplace(document.querySelector(querySelector), vNode)

  @deprecated("Use renderInto instead (or renderReplace)", "0.11.0")
  def render(querySelector: String, vNode: VNode)(implicit s: Scheduler): IO[Unit] = renderInto(querySelector, vNode)

  def renderWithStore[S, A](initialState: S, reducer: (S, svg.A) => (S, Option[IO[svg.A]]), querySelector: String, root: VNode
  )(implicit s: Scheduler): IO[Unit] =
    Store.renderWithStore(initialState, reducer, querySelector, root)
}

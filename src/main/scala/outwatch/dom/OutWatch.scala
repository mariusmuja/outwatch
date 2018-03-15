package outwatch.dom

import cats.effect.{Effect, IO, Sync}
import cats.syntax.all._
import monix.execution.Scheduler
import org.scalajs.dom
import org.scalajs.dom._
import outwatch.util.Store
import snabbdom.patch

object OutWatch {

  def renderInto[F[+_]: Effect](element: dom.Element, vNode: VNodeF[F])(implicit s: Scheduler): F[Unit] = for {
    node <- vNode
    _ <- Sync[F].delay {
      val elem = dom.document.createElement("app")
      element.appendChild(elem)
      patch(elem, node.toSnabbdom)
    }
  } yield ()

  def renderReplace[F[+_]: Effect](element: dom.Element, vNode: VNodeF[F])(implicit s: Scheduler): F[Unit] = for {
    node <- vNode
    _ <- Sync[F].delay(patch(element, node.toSnabbdom))
  } yield ()

  def renderInto[F[+_]: Effect](querySelector: String, vNode: VNodeF[F])(implicit s: Scheduler): F[Unit] =
    renderInto[F](document.querySelector(querySelector), vNode)

  def renderReplace[F[+_]: Effect](querySelector: String, vNode: VNodeF[F])(implicit s: Scheduler): F[Unit] =
    renderReplace[F](document.querySelector(querySelector), vNode)

  @deprecated("Use renderInto instead (or renderReplace)", "0.11.0")
  def render(querySelector: String, vNode: VNode)(implicit s: Scheduler): IO[Unit] =
    renderInto[IO](querySelector, vNode)

  def renderWithStore[S, A](initialState: S, reducer: (S, svg.A) => (S, Option[IO[svg.A]]), querySelector: String, root: VNode
  )(implicit s: Scheduler): IO[Unit] =
    Store.renderWithStore(initialState, reducer, querySelector, root)
}

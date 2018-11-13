package outwatch.util

import monix.execution.Cancelable.IsDummy
import monix.execution.cancelables.CompositeCancelable
import org.scalajs.dom
import outwatch.dom.helpers.STRef
import outwatch.dom.{IO, Observable, OutWatch, VNode}
import outwatch.{Handler, Pipe}

import scala.util.Try
import scala.util.control.NonFatal


object Store {

  case class Reducer[S, A](reducer: (S, A) => (S, Observable[A]))

  object Reducer {

    implicit def stateAndEffects[S, A](f: (S, A) => (S, Observable[A])): Reducer[S, A] = Reducer(f)

    implicit def justState[S, A](f: (S, A) => S): Reducer[S, A] = Reducer { (s: S, a: A) => (f(s, a), Observable.empty) }

    implicit def stateAndOptionIO[S, A](f: (S, A) => (S, Option[IO[A]])): Reducer[S, A] =  Reducer { (s: S, a: A) =>
      val (mewState, effect) = f(s, a)
      (mewState, effect.fold[Observable[A]](Observable.empty)(Observable.fromTaskLike))
    }
  }

  private val storeRef = STRef.empty

  def create[State, Action](
    initialState: State,
    reducer: Reducer[State, Action]
  ): IO[Pipe[Action, State]] = {

    IO.deferAction { implicit scheduler =>
      Handler.create[Action].map { handler =>

        val sub = CompositeCancelable()
        val fold: (State, Action) => State = (state, action) => Try { // guard against reducer throwing an exception
          val (newState, effects) = reducer.reducer(state, action)

          val cancelable = effects.subscribe(
            e => handler.feed(e :: Nil),
            e => dom.console.error(e.getMessage) // just log the error, don't push it into the handler's observable, because it would stop the scan "loop"
          )
          if (!cancelable.isInstanceOf[IsDummy]) sub += cancelable
          newState
        }.recover { case NonFatal(e) =>
          dom.console.error(e.getMessage)
          state
        }.get

        handler.transformSource(source =>
          source
            .scan(initialState)(fold)
            .behavior(initialState).refCount
            .doOnSubscriptionCancelF(() => sub.cancel())
        )
      }
    }
  }

  def get[S, A]: IO[Pipe[A, S]] = storeRef.asInstanceOf[STRef[Pipe[A, S]]].getOrThrow(NoStoreException)

  def renderWithStore[S, A](
    initialState: S, reducer: Reducer[S, A], selector: String, root: VNode
  ): IO[Unit] = for {
    store <- Store.create(initialState, reducer)
    _ <- storeRef.asInstanceOf[STRef[Pipe[A, S]]].put(store)
    _ <- OutWatch.renderInto(selector, root)
  } yield ()

  private object NoStoreException
    extends Exception("Application was rendered without specifying a Store, please use Outwatch.renderWithStore instead")
}

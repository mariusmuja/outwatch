package outwatch

import cats.effect.implicits._
import cats.effect.{Effect, IO}
import minitest.TestSuite
import minitest.api.{SourceLocation, Void}
import monix.eval.Task
import monix.execution.Ack.Continue
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.schedulers.TrampolineScheduler
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom.document
import outwatch.dom.Observable

import scala.concurrent.{Future, Promise}


trait EasySubscribe {
  implicit class Subscriber[T](obs: Observable[T]) {
    def apply(next: T => Unit)(implicit s: Scheduler): Cancelable = obs.subscribe { t =>
      next(t)
      Continue
    }
  }
}

trait TestDSL { self: TestSuite[_] =>

  implicit class ShouldBe[T](received: => T) {
    def shouldBe(expected: => T)(implicit pos: SourceLocation): Unit = assertEquals(received, expected)
    def shouldNotBe(expected: => T)(implicit pos: SourceLocation): Unit = assert(received != expected)
  }
}


trait JSDomSuite extends TestSuite[Unit]
                 with EasySubscribe
                 with TestDSL { self =>

  implicit val scheduler: Scheduler = TrampolineScheduler(Scheduler.global, SynchronousExecution)

  // hack-ish way to add unsafeRunSync to not have to re-write most tests right away
  implicit class TaskExt[T](task: Task[T]) {
    var taskValue: T = _
    def unsafeRunSync(): T = {
      task.runAsync {
        case Right(value) => taskValue = value
        case Left(ex) => println(ex.getMessage)
      }
      taskValue
    }
  }

  def setup(): Unit = {
    document.body.innerHTML = ""

    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  def tearDown(env: Unit): Unit = {}

  def test(name: String)(f: => Void): Unit = super.test(name)(_ => f)

  def testEffect[F[_]: Effect](name: String)(f: F[Unit]): Unit = {
    val p = Promise[Unit]
    f.runAsync(cb => IO(cb.fold[Unit](p.failure, p.success))).unsafeRunSync()
    super.testAsync(name)(_ => p.future)
  }

  def testAsync(name: String)(f: => Future[Unit]): Unit = super.testAsync(name)(_ => f)
}

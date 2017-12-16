package outwatch

import minitest.TestSuite
import monix.execution.Ack.Continue
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.schedulers.TrampolineScheduler
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.scalajs.dom.document

import scala.concurrent.Future


trait EasySubscribe {
  implicit class Subscriber[T](obs: Observable[T]) {
    def apply(next: T => Unit)(implicit s: Scheduler): Cancelable = obs.subscribe { t =>
      next(t)
      Continue
    }
  }
}

trait JSDomSuite extends TestSuite[Unit] with EasySubscribe {

  implicit val scheduler = Scheduler.global
  val trampolineScheduler = TrampolineScheduler(scheduler, SynchronousExecution)

  def setup(): Unit = {
    document.body.innerHTML = ""

    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  def tearDown(env: Unit): Unit = {}

  def test(name: String)(f: => Unit): Unit = super.test(name)(_ => f)

  def testAsync(name: String)(f: => Future[Unit]): Unit = super.testAsync(name)(_ => f)
}

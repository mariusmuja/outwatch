package outwatch


import minitest.TestSuite
import minitest.api.Void
import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.scalajs.dom._

import scala.concurrent.Future


trait EasySubscribe {
  implicit val executionContext: Scheduler = monix.execution.Scheduler.Implicits.global

  implicit class Subscriber[T](obs: Observable[T]) {
    def apply(next: T => Unit): Cancelable = obs.subscribe { t =>
      next(t)
      Continue
    }
  }

}


trait JSDomSuite extends TestSuite[Unit] with EasySubscribe {

  def setup(): Unit = {
    document.body.innerHTML = ""

    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  def tearDown(env: Unit): Unit = {}

  def test(name: String)(f: => Void): Unit = super.test(name)(_ => f)

  def testAsync(name: String)(f: => Future[Unit]): Unit = super.testAsync(name)(_ => f)
}

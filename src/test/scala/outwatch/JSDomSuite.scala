package outwatch

import minitest.TestSuite
import minitest.api.{SourceLocation, Void}
import monix.execution.Ack.Continue
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.schedulers.TrampolineScheduler
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom.{document, window}
import outwatch.Deprecated.IgnoreWarnings.initEvent
import outwatch.dom.Observable

import scala.concurrent.Future


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


trait LocalStorageHelper {
  import scala.scalajs.js

  def dispatchStorageEvent(key: String, newValue: String, oldValue: String): Unit = {
    if (key == null) window.localStorage.clear()
    else window.localStorage.setItem(key, newValue)

    val event = document.createEvent("Events")
    initEvent(event)("storage", canBubbleArg = true, cancelableArg = false)
    event.asInstanceOf[js.Dynamic].key = key
    event.asInstanceOf[js.Dynamic].newValue = newValue
    event.asInstanceOf[js.Dynamic].oldValue = oldValue
    event.asInstanceOf[js.Dynamic].storageArea = window.localStorage
    window.dispatchEvent(event)
    ()
  }
}


trait JSDomSuite extends TestSuite[Unit]
                 with EasySubscribe
                 with TestDSL
                 with LocalStorageHelper {

  implicit val scheduler: Scheduler = TrampolineScheduler(Scheduler.global, SynchronousExecution)

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

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

// TODO: We need this mock until localStorage is implemented in jsdom (https://github.com/tmpvar/jsdom/pull/2076)
trait LocalStorageMock {
  import scala.collection.mutable
  import scala.scalajs.js


  if (js.isUndefined(window.localStorage)) {
    js.Dynamic.global.window.updateDynamic("localStorage")(new js.Object {
      private val map = new mutable.HashMap[String, String]

      def getItem(key: String): String = map.getOrElse(key, null)

      def setItem(key: String, value: String): Unit = {
        map += key -> value
      }

      def removeItem(key: String): Unit = {
        map -= key
      }

      def clear(): Unit = map.clear()
    })
  }

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



trait JSDomSuite extends TestSuite[Unit] with EasySubscribe with TestDSL with LocalStorageMock {

  implicit val scheduler = TrampolineScheduler(Scheduler.global, SynchronousExecution)

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

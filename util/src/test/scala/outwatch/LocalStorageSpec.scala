package outwatch

import org.scalajs.dom.window.localStorage
import org.scalajs.dom.{document, window}
import outwatch.EventExt._

import scala.collection.mutable



trait LocalStorageHelper {
  import scala.scalajs.js

  def dispatchStorageEvent(key: String, newValue: String, oldValue: String): Unit = {
    if (key == null) window.localStorage.clear()
    else window.localStorage.setItem(key, newValue)

    val event = document.createEvent("Events")
    event.initEvent("storage", canBubbleArg = true, cancelableArg = false)
    event.asInstanceOf[js.Dynamic].key = key
    event.asInstanceOf[js.Dynamic].newValue = newValue
    event.asInstanceOf[js.Dynamic].oldValue = oldValue
    event.asInstanceOf[js.Dynamic].storageArea = window.localStorage
    window.dispatchEvent(event)
    ()
  }
}



object LocalStorageSpec extends JSDomSuite with LocalStorageHelper {

  test("LocalStorage should provide a handler") {

    val key = "banana"
    val triggeredHandlerEvents = mutable.ArrayBuffer.empty[Option[String]]

    localStorage.getItem(key) shouldBe null

    val storageHandler = util.LocalStorage.handler(key).unsafeRunSync()
    storageHandler.foreach { e => triggeredHandlerEvents += e }
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None)

    storageHandler.onNext(Some("joe"))
    localStorage.getItem(key) shouldBe "joe"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"))

    var initialValue: Option[String] = null
    util.LocalStorage.handler(key).unsafeRunSync().foreach {
      initialValue = _
    }
    initialValue shouldBe Some("joe")

    storageHandler.onNext(None)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None)

    // localStorage.setItem(key, "split") from another window
    dispatchStorageEvent(key, newValue = "split", null)
    localStorage.getItem(key) shouldBe "split"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"))

    // localStorage.removeItem(key) from another window
    dispatchStorageEvent(key, null, "split")
    localStorage.getItem(key) shouldBe "null"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None)

    // only trigger handler if value changed
    storageHandler.onNext(None)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None)

    storageHandler.onNext(Some("rhabarbar"))
    localStorage.getItem(key) shouldBe "rhabarbar"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar"))

    // localStorage.clear() from another window
    dispatchStorageEvent(null, null, null)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar"), None)
  }


}

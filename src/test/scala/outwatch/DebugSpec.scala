package outwatch

import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.document
import outwatch.dom.OutWatch
import outwatch.dom.dsl._

object DebugSpec extends JSDomSuite{
//
//  test("The HTML DSL should update merged node attributes correctly") {
//    val messages = PublishSubject[String]
//    val otherMessages = PublishSubject[String]
//    val vNode = div(data.noise <-- messages)(data.noise <-- otherMessages)
//
//    val node = document.createElement("div")
//    document.body.appendChild(node)
//    OutWatch.renderInto(node, vNode).unsafeRunSync()
//
//    otherMessages.onNext("otherMessage")
//    node.children(0).getAttribute("data-noise") shouldBe "otherMessage"
//
//    messages.onNext("message")
//    node.children(0).getAttribute("data-noise") shouldBe "message"
//
//    otherMessages.onNext("genus")
//    node.children(0).getAttribute("data-noise") shouldBe "genus"
//  }

}

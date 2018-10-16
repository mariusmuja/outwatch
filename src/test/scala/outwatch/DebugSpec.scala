package outwatch

import monix.reactive.Observable
import org.scalajs.dom.document
import outwatch.dom.dsl._
import outwatch.dom.{OutWatch, VDomModifier}

object DebugSpec extends JSDomSuite{

//  test("Modifier stream should work for multiple nested modifier stream receiver") {
//    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
//    val node = div(id := "strings",
//      div(myHandler)
//    )
//
//    OutWatch.renderInto("#app", node).unsafeRunSync()
//
//    val element = document.getElementById("strings")
//    element.innerHTML shouldBe "<div></div>"
//
//    myHandler.unsafeOnNext(
//      Observable[VDomModifier](
//        VDomModifier(
//          Observable[VDomModifier]("a"),
//          Observable(span("b"))
//        )
//      )
//    )
//    element.innerHTML shouldBe """<div>a<span>b</span></div>"""
//  }

}

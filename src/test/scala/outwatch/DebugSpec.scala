package outwatch

import org.scalajs.dom.document
import outwatch.dom.OutWatch
import outwatch.dom.dsl.{cls, div, id, modifiers}

object DebugSpec extends JSDomSuite{

  test("Modifier stream should work for streaming accum attributes") {
    val myClasses = Handler.create[String]("second").unsafeRunSync()
    val myClasses2 = Handler.create[String]().unsafeRunSync()
    val node = div(
      id := "strings",
      div(
        cls := "first",
        myClasses.map { cls := _ },
//        modifiers(
          cls <-- myClasses2
//        )
      )
    )
    OutWatch.renderInto("#app", node).unsafeRunSync()
    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div class="first second"></div>"""
    myClasses2.unsafeOnNext("third")
    element.innerHTML shouldBe """<div class="first second third"></div>"""
    myClasses2.unsafeOnNext("more")
    element.innerHTML shouldBe """<div class="first second more"></div>"""
    myClasses.unsafeOnNext("yeah")
    element.innerHTML shouldBe """<div class="first yeah more"></div>"""
  }

}

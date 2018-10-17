package outwatch

object DebugSpec extends JSDomSuite{

//    test("Modifier stream should work for nested observables with seq modifiers and attribute stream") {
//      val innerHandler = Handler.create[String]().unsafeRunSync()
//      val outerHandler = Handler.create(Seq[VDomModifier]("a", data.test := "v", href <-- innerHandler)).unsafeRunSync()
//      val node = div(
//        id := "strings",
//        outerHandler
//      )
//
//      OutWatch.renderInto("#app", node).unsafeRunSync()
//
//      val element = document.getElementById("strings")
//      element.outerHTML shouldBe """<div id="strings" data-test="v">a</div>"""
//
//      innerHandler.unsafeOnNext("c")
//      element.outerHTML shouldBe """<div id="strings" data-test="v" href="c">a</div>"""
//
//      innerHandler.unsafeOnNext("d")
//      element.outerHTML shouldBe """<div id="strings" data-test="v" href="d">a</div>"""
//
//      outerHandler.unsafeOnNext(Seq[VDomModifier]("meh"))
//      element.outerHTML shouldBe """<div id="strings">meh</div>"""
//    }

}

package outwatch

import monix.execution.Ack.Continue
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import outwatch.dom._
import outwatch.dom.dsl._

import scala.collection.mutable


object LifecycleHookSpec extends JSDomSuite {

  test("Insertion hooks should be called correctly") {

    var switch = false
    val sink = Sink.create{_: Element =>
      switch = true
      Continue
    }

    val node = sink.flatMap { sink =>
      div(key := 1, onInsert --> sink)
    }

    switch shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
  }


  test("Insertion hooks should be called correctly on merged nodes") {
    var switch = false
    val sink = Sink.create{_: Element =>
      switch = true
      Continue
    }
    var switch2 = false
    val sink2 = Sink.create{_: Element =>
      switch2 = true
      Continue
    }

    val node = for {
      sink <- sink
      sink2 <- sink2
      node <- div(key := 1, onInsert --> sink)(onInsert --> sink2)
    } yield node

    switch shouldBe false
    switch2 shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
    switch2 shouldBe true
  }


  test("Destruction hooks should be called correctly") {

    var switch = false
    val sink = Sink.create{_: Element =>
      switch = true
      Continue
    }

    val node = sink.flatMap { sink =>
      div(Observable(span(onDestroy --> sink), div("Hasdasd")))
    }

    switch shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
  }


  test("Destruction hooks should be called correctly on merged nodes") {

    var switch = false
    val sink = Sink.create{_: Element =>
      switch = true
      Continue
    }
    var switch2 = false
    val sink2 = Sink.create{_: Element =>
      switch2 = true
      Continue
    }

    val node = for {
      sink <- sink
      sink2 <- sink2
      node <- div(Observable(span(onDestroy --> sink)(onDestroy --> sink2), div("Hasdasd")))
    } yield node

    switch shouldBe false
    switch2 shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
    switch2 shouldBe true
  }


  test("Update hooks should be called correctly on merged nodes") {
    var switch1 = false
    val sink1 = Sink.create{_: (Element, Element) =>
      switch1 = true
      Continue
    }
    var switch2 = false
    val sink2 = Sink.create{_: (Element, Element) =>
      switch2 = true
      Continue
    }

    val message = PublishSubject[String]
    val node = for {
      sink1 <- sink1
      sink2 <- sink2
      node <- div(message, onUpdate --> sink1)(onUpdate --> sink2)
    } yield node

    OutWatch.renderInto("#app", node).unsafeRunSync()
    switch1 shouldBe false
    switch2 shouldBe false

    message.onNext("wursi")
    switch1 shouldBe true
    switch2 shouldBe true
  }

  test("Update hooks should be called correctly") {

    var switch = false
    val sink = Sink.create{_: (Element, Element) =>
      switch = true
      Continue
    }

    val node = sink.flatMap { sink =>
      div(Observable(span(onUpdate --> sink, "Hello"), span(onUpdate --> sink, "Hey")))
    }

    switch shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
  }



  test("Prepatch hooks should be called") {

    var switch = false
    val sink = Sink.create{_: (Option[Element], Option[Element]) =>
      switch = true
      Continue
    }

    val node = sink.flatMap { sink =>
      div(Observable(span("Hello")), span(attributes.key := "1", onPrePatch --> sink, "Hey"))
    }

    switch shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
  }

  test("Prepatch hooks should be called correctly on merged nodes") {
    var switch1 = false
    val sink1 = Sink.create{_: (Option[Element], Option[Element]) =>
      switch1 = true
      Continue
    }
    var switch2 = false
    val sink2 = Sink.create{_: (Option[Element], Option[Element]) =>
      switch2 = true
      Continue
    }
    val message = PublishSubject[String]()
    val node =  for {
      sink1 <- sink1
      sink2 <- sink2
      node <- div(message, onPrePatch --> sink1)(onPrePatch --> sink2)
    } yield node

    OutWatch.renderInto("#app", node).unsafeRunSync()
    switch1 shouldBe false
    switch2 shouldBe false

    message.onNext("wursi")

    switch1 shouldBe true
    switch2 shouldBe true
  }

  test("Postpatch hooks should be called") {

    var switch = false
    val sink = Sink.create{_: (Element, Element) =>
      switch = true
      Continue
    }

    val node = sink.flatMap { sink =>
      div(Observable.pure("message"), onPostPatch --> sink, "Hey")
    }

    switch shouldBe false

    OutWatch.renderInto("#app", node).unsafeRunSync()

    switch shouldBe true
  }


  test("Postpatch hooks should be called correctly on merged nodes") {
    var switch1 = false
    val sink1 = Sink.create{_: (Element, Element) =>
      switch1 = true
      Continue
    }
    var switch2 = false
    val sink2 = Sink.create{_: (Element, Element) =>
      switch2 = true
      Continue
    }
    val message = PublishSubject[String]()
    val node = for {
      sink1 <- sink1
      sink2 <- sink2
      node <- div(message, onPostPatch --> sink1)(onPostPatch --> sink2)
    } yield node

    OutWatch.renderInto("#app", node).unsafeRunSync()
    switch1 shouldBe false
    switch2 shouldBe false

    message.onNext("wursi")

    switch1 shouldBe true
    switch2 shouldBe true
  }


  private def createLifecycleHooks: (Seq[String], VDomModifier) = {

    val hooks = mutable.ArrayBuffer.empty[String]
    val insertSink = Sink.create { _: Element =>
      hooks += "insert"
      Continue
    }
    val prepatchSink = Sink.create { _: (Option[Element], Option[Element]) =>
      hooks += "prepatch"
      Continue
    }
    val updateSink = Sink.create { _: (Element, Element) =>
      hooks += "update"
      Continue
    }
    val postpatchSink = Sink.create { _: (Element, Element) =>
      hooks += "postpatch"
      Continue

    }
    val destroySink = Sink.create { _: Element =>
      hooks += "destroy"
      Continue
    }

    val logSinks = for {
      insertSink <- insertSink
      updateSink <- updateSink
      destroySink <- destroySink
      prepatchSink <- prepatchSink
      postpatchSink <- postpatchSink
      mod <- modifiers(
        onInsert --> insertSink,
        onPrePatch --> prepatchSink,
        onUpdate --> updateSink,
        onPostPatch --> postpatchSink,
        onDestroy --> destroySink
      )
    } yield mod

    (hooks, logSinks)
  }


  test("Hooks should be called in the correct order for modified node") {
    val (hooks, lifecycleHooks) = createLifecycleHooks

    val message = PublishSubject[String]()
    val node = div(message, lifecycleHooks)

    hooks.toList shouldBe List.empty

    OutWatch.renderInto("#app", node).unsafeRunSync()

    hooks.toList shouldBe List("insert")

    message.onNext("next")

    hooks.toList shouldBe List("insert", "prepatch", "update", "postpatch")
  }


  test("Empty single children receiver should not trigger node update on render") {
    val (hooks, lifecycleHooks) = createLifecycleHooks

    val messageList = PublishSubject[Seq[String]]()
    val node = div("Hello", messageList.map(_.map(span(_))), lifecycleHooks)

    hooks.toList shouldBe List.empty

    OutWatch.renderInto("#app", node).unsafeRunSync()

    hooks.toList shouldBe  List("insert")
  }

  test("Static child nodes should not be destroyed and inserted when child stream emits") {
    val (hooks, lifecycleHooks) = createLifecycleHooks

    val message = PublishSubject[String]()
    val node = div(span("Hello", lifecycleHooks),
      message.map(span(_))
    )

    hooks.toList shouldBe List.empty

    OutWatch.renderInto("#app", node).unsafeRunSync()

    message.onNext("next")

    hooks.contains("destroy") shouldBe false
  }

  test("Static child nodes should be only inserted once when children stream emits") {
    val (hooks, lifecycleHooks) = createLifecycleHooks

    val messageList = PublishSubject[Seq[String]]()
    val node = div(
      messageList.map(_.map(span(_))),
      span("Hello", lifecycleHooks)
    )

    hooks.toList shouldBe List.empty

    OutWatch.renderInto("#app", node).unsafeRunSync()

    messageList.onNext(Seq("one"))

    messageList.onNext(Seq("one", "two"))

    hooks.count(_ == "insert") shouldBe 1
  }



  test("Managed subscriptions should unsubscribe on destroy") {

    val nodes = PublishSubject[VNode]

    var latest = ""
    val sink = Sink.create { elem: String =>
      latest = elem
      Continue
    }

    val sub = PublishSubject[String]

    val node = sink.flatMap { sink =>
      div(nodes.startWith(Seq(
        span(managed(sink <-- sub))
      )))
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    latest shouldBe ""

    sub.onNext("first")
    latest shouldBe "first"

    nodes.onNext(div()) // this triggers child destroy and subscription cancelation

    sub.onNext("second")
    latest shouldBe "first"
  }


  test("Hooks should support emitter operations") {

    val operations = mutable.ArrayBuffer.empty[String]

    val sink = Sink.create { op: String =>
      operations += op
      Continue
    }

    val divTagName = onInsert.map(_.tagName.toLowerCase).filter(_ == "div")

    val node = sink.flatMap { sink =>
      div(key := 1, onInsert("insert") --> sink,
        div(divTagName --> sink),
        span(divTagName --> sink)
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    operations.toList shouldBe List("div", "insert")
  }

  //  import org.scalajs.{dom => dm}
//
//  def debug(msg: String) = VDomModifier(
//    Sink.create[dm.Element] { _ => dm.console.log("Insert: " + msg); Continue }.flatMap(onInsert --> _),
//    Sink.create[dm.Element] { _ => dm.console.log("Destroy: " + msg); Continue }.flatMap(onDestroy --> _),
//    Sink.create[(dm.Element, dm.Element)] { _ => dm.console.log("PostPatch: " + msg); Continue}.flatMap(onPostPatch --> _),
//    //    Sink.create[dom.Element] { _ => dom.console.log("Insert: " + msg); Continue }.flatMap(onDomMount --> _),
//    //    Sink.create[dom.Element] { _ => dom.console.log("Destroy: " + msg); Continue }.flatMap(onDomUnmount --> _),
//    //    Sink.create[dom.Element] { _ => dom.console.log("PostPatch: " + msg); Continue}.flatMap(onDomUpdate --> _),
//  )

  test("Hooks properly unsubscribe streams after nodes are patched") {

    val (hooks, lifecycleHooks) = createLifecycleHooks

    val tab = PublishSubject[Int]
    val child = PublishSubject[Int]

    def page(num: Int) = num match {
      case 1 =>
        div("Test 1",
          div(
            "Child: ",
            child.map { ch =>
              div(s"Child: $ch", lifecycleHooks)
            }
          )
        )
      case 2 => div(
        div("Test 2")
      )
    }

    val node = div(
      tab.map(page)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    tab.onNext(2)
    tab.onNext(1)

    hooks.count(_ == "insert") shouldBe 0
    hooks.count(_ == "postpatch") shouldBe 0
    hooks.count(_ == "destroy") shouldBe 0

    child.onNext(1)

    hooks.count(_ == "insert") shouldBe 1
    hooks.count(_ == "postpatch") shouldBe 0
    hooks.count(_ == "destroy") shouldBe 0

    child.onNext(2)

    hooks.count(_ == "insert") shouldBe 1
    hooks.count(_ == "postpatch") shouldBe 1
    hooks.count(_ == "destroy") shouldBe 0

    tab.onNext(2)

    hooks.count(_ == "insert") shouldBe 1
    hooks.count(_ == "postpatch") shouldBe 1
    hooks.count(_ == "destroy") shouldBe 1

  }

}

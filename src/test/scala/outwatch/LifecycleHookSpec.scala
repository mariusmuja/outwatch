package outwatch

import cats.effect.IO
import minitest.TestSuite
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import outwatch.dom._

import scala.collection.mutable


object LifecycleHookSpec extends TestSuite[Unit] {

  def setup(): Unit = {
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  def tearDown(env: Unit): Unit = {
    document.body.innerHTML = ""
  }


  test("Insertion hooks should be called correctly") { _ =>

    var switch = false
    val sink = Sink.create((_: Element) => IO {
      switch = true
      Continue
    })

    val node = div(insert --> sink)

    assertEquals(switch, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
  }


  test("Insertion hooks should be called correctly on merged nodes") { _ =>
    var switch = false
    val sink = Sink.create((_: Element) => IO{
      switch = true
      Continue
    })
    var switch2 = false
    val sink2 = Sink.create((_: Element) => IO{
      switch2 = true
      Continue
    })

    val node = div(insert --> sink)(insert --> sink2)

    assertEquals(switch, false)
    assertEquals(switch2, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
    assertEquals(switch2, true)
  }


  test("Destruction hooks should be called correctly") { _ =>

    var switch = false
    val sink = Sink.create((_: Element) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable[VNode](span(destroy --> sink), "Hasdasd"))

    assertEquals(switch, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
  }


  test("Destruction hooks should be called correctly on merged nodes") { _ =>

    var switch = false
    val sink = Sink.create((_: Element) => IO{
      switch = true
      Continue
    })
    var switch2 = false
    val sink2 = Sink.create((_: Element) => IO{
      switch2 = true
      Continue
    })

    val node = div(child <-- Observable[VNode](span(destroy --> sink)(destroy --> sink2), "Hasdasd"))

    assertEquals(switch, false)
    assertEquals(switch2, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
    assertEquals(switch2, true)
  }


  test("Update hooks should be called correctly") { _ =>

    var switch = false
    val sink = Sink.create((_: (Element, Element)) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable(span(update --> sink, "Hello"), span(update --> sink, "Hey")))

    assertEquals(switch, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
  }


  test("Update hooks should be called correctly on merged nodes") { _ =>
    var switch1 = false
    val sink1 = Sink.create((_: (Element, Element)) => IO{
      switch1 = true
      Continue
    })
    var switch2 = false
    val sink2 = Sink.create((_: (Element, Element)) => IO{
      switch2 = true
      Continue
    })

    val message = PublishSubject[String]
    val node = div(child <-- message, update --> sink1)(update --> sink2)

    OutWatch.render("#app", node).unsafeRunSync()
    assertEquals(switch1, false)
    assertEquals(switch2, false)

    message.onNext("wursi")
    assertEquals(switch1, true)
    assertEquals(switch2, true)
  }


  test("Prepatch hooks should be called") { _ =>

    var switch = false
    val sink = Sink.create((_: (Option[Element], Option[Element])) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable(span("Hello")), span(prepatch --> sink, "Hey"))

    assertEquals(switch, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
  }

  test("Prepatch hooks should be called correctly on merged nodes") { _ =>
    var switch1 = false
    val sink1 = Sink.create((_: (Option[Element], Option[Element])) => IO{
      switch1 = true
      Continue
    })
    var switch2 = false
    val sink2 = Sink.create((_: (Option[Element], Option[Element])) => IO{
      switch2 = true
      Continue
    })
    val message = PublishSubject[String]()
    val node = div(child <-- message, prepatch --> sink1)(prepatch --> sink2)

    OutWatch.render("#app", node).unsafeRunSync()
    assertEquals(switch1, false)
    assertEquals(switch2, false)

    message.onNext("wursi")
    assertEquals(switch1, true)
    assertEquals(switch2, true)
  }

  test("Postpatch hooks should be called") { _ =>

    var switch = false
    val sink = Sink.create((_: (Element, Element)) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable("message"), postpatch --> sink, "Hey")

    assertEquals(switch, false)

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(switch, true)
  }


  test("Postpatch hooks should be called correctly on merged nodes") { _ =>
    var switch1 = false
    val sink1 = Sink.create((_: (Element, Element)) => IO{
      switch1 = true
      Continue
    })
    var switch2 = false
    val sink2 = Sink.create((_: (Element, Element)) => IO{
      switch2 = true
      Continue
    })
    val message = PublishSubject[String]()
    val node = div(child <-- message, postpatch --> sink1)(postpatch --> sink2)

    OutWatch.render("#app", node).unsafeRunSync()
    assertEquals(switch1, false)
    assertEquals(switch2, false)

    message.onNext("wursi")
    assertEquals(switch1, true)
    assertEquals(switch2, true)
  }


  test("Hooks should be called in the correct order for modified node") { _ =>
    val hooks = mutable.ArrayBuffer.empty[String]
    val insertSink = Sink.create((_: Element) => IO {hooks += "insert"; Continue})
    val prepatchSink = Sink.create((_: (Option[Element], Option[Element])) => IO{hooks += "prepatch"; Continue} )
    val updateSink = Sink.create((_: (Element, Element)) => IO{hooks += "update"; Continue} )
    val postpatchSink = Sink.create((_: (Element, Element)) => IO{hooks += "postpatch"; Continue} )
    val destroySink = Sink.create((_: Element) => IO{hooks += "destroy"; Continue} )

    val message = PublishSubject[String]()
    val node = div(child <-- message,
      insert --> insertSink,
      prepatch --> prepatchSink,
      update --> updateSink,
      postpatch --> postpatchSink,
      destroy --> destroySink
    )

    assertEquals(hooks.toList, List())

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(hooks.toList, List("insert"))

    message.onNext("next")

    assertEquals(hooks.toList, List("insert", "prepatch", "update", "postpatch"))
  }


  test("Empty single children receiver should not trigger node update on render") { _ =>
    val hooks = mutable.ArrayBuffer.empty[String]
    val insertSink = Sink.create((_: Element) => IO {hooks += "insert"; Continue})
    val updateSink = Sink.create((_: (Element, Element)) => IO{hooks += "update"; Continue} )

    val messageList = PublishSubject[Seq[String]]()
    val node = div("Hello",  children <-- messageList.map(_.map(span(_))),
      insert --> insertSink,
      update --> updateSink
    )

    assertEquals(hooks.toList, List())

    OutWatch.render("#app", node).unsafeRunSync()

    assertEquals(hooks.toList, List("insert"))
  }

  test("Static child nodes should not be destroyed and inserted when child stream emits") { _ =>
    val hooks = mutable.ArrayBuffer.empty[String]
    val insertSink = Sink.create((_: Element) => IO {hooks += "insert"; Continue})
    val updateSink = Sink.create((_: (Element, Element)) => IO{hooks += "update"; Continue} )
    val destroySink = Sink.create((_: Element) => IO {hooks += "destroy"; Continue})

    val message = PublishSubject[String]()
    val node = div(span("Hello", insert --> insertSink, update --> updateSink,destroy --> destroySink),
      child <-- message.map(span(_))
    )

    assertEquals(hooks.toList, List())

    OutWatch.render("#app", node).unsafeRunSync()

    message.onNext("next")

    assert(!hooks.contains("destroy"), "Static child node destroyed")
  }

  test("Static child nodes should be only inserted once when children stream emits") { _ =>
    val hooks = mutable.ArrayBuffer.empty[String]
    val insertSink = Sink.create((_: Element) => IO {hooks += "insert"; Continue})
    val updateSink = Sink.create((_: (Element, Element)) => IO{hooks += "update"; Continue} )
    val destroySink = Sink.create((_: Element) => IO {hooks += "destroy"; Continue})

    val messageList = PublishSubject[Seq[String]]()
    val node = div(children <-- messageList.map(_.map(span(_))),
      span("Hello", insert --> insertSink, update --> updateSink,destroy --> destroySink)
    )

    assertEquals(hooks.toList, List())

    OutWatch.render("#app", node).unsafeRunSync()

    messageList.onNext(Seq("one"))

    messageList.onNext(Seq("one", "two"))

    val count = hooks.count(_ == "insert")
    assert(count == 1, s"Static child node inserted $count times")
  }

}

package outwatch

import cats.effect.IO
import minitest.TestSuite
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import outwatch.dom._


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

}

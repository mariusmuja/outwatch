package outwatch

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import org.scalatest.BeforeAndAfterEach
import outwatch.dom._


class LifecycleHookSpec extends UnitSpec with BeforeAndAfterEach {

  override def beforeEach(): Unit = {
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  override def afterEach(): Unit = {
    document.body.innerHTML = ""
  }

  "Insertion hooks" should "be called correctly" in {

    var switch = false
    val sink = Sink.create((_: Element) => IO {
      switch = true
      Continue
    })

    val node = div(insert --> sink)

    switch shouldBe false

    OutWatch.render("#app", node).unsafeRunSync()

    switch shouldBe true

  }

  it should "be called correctly on merged nodes" in {
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

    switch shouldBe false
    switch2 shouldBe false

    OutWatch.render("#app", node).unsafeRunSync()

    switch shouldBe true
    switch2 shouldBe true

  }

  "Destruction hooks" should "be called correctly on merged nodes" in {

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

    val node = div(child <-- Observable(span(destroy --> sink)(destroy --> sink2), "Hasdasd"))

    switch shouldBe false
    switch2 shouldBe false

    OutWatch.render("#app", node).unsafeRunSync()

    switch shouldBe true
    switch2 shouldBe true

  }

  it should "be called correctly" in {

    var switch = false
    val sink = Sink.create((_: Element) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable(span(destroy --> sink), "Hasdasd"))

    switch shouldBe false

    OutWatch.render("#app", node).unsafeRunSync()

    switch shouldBe true

  }

  "Update hooks" should "be called correctly on merged nodes" in {
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
    switch1 shouldBe false
    switch2 shouldBe false

    message.onNext("wursi")
    switch1 shouldBe true
    switch2 shouldBe true
  }


  it should "be called correctly" in {

    var switch = false
    val sink = Sink.create((_: (Element, Element)) => IO {
      switch = true
      Continue
    })

    val node = div(child <-- Observable(span(update --> sink, "Hello"), span(update --> sink, "Hey")))

    switch shouldBe false

    OutWatch.render("#app", node).unsafeRunSync()

    switch shouldBe true

  }

}

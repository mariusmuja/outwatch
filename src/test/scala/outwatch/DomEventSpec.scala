package outwatch

import monix.eval.Task
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLInputElement, MouseEvent}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterEach, Matchers}
import org.scalatest.prop.PropertyChecks
import io.monadless.monix.MonadlessTask._

class DomEventSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterEach with PropertyChecks {

  override implicit val executionContext = monix.execution.Scheduler.Implicits.global

  override def beforeEach(): Unit = {
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  override def afterEach(): Unit = {
    document.body.innerHTML = ""
  }

  "EventStreams" should "emit and receive events correctly" in {
    import outwatch.dom._

    val vtree = for {
      observable <- createMouseHandler()
      buttonDisabled = observable.map(_ => true).startWith(Seq(false))
    } yield div(id :="click", click --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )


    vtree.flatMap(vtree => OutWatch.render("#app", vtree)).unsafeRunSync()
    document.getElementById("btn").hasAttribute("disabled") shouldBe false

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    Task(document.getElementById("btn").getAttribute("disabled") shouldBe "").runAsync
  }

  it should "be converted to a generic emitter correctly" in {
    import outwatch.dom._

    val message = "ad"

    val vtree = for {
      observable <- createStringHandler()
    } yield div(id := "click", click(message) --> observable,
        span(id := "child", child <-- observable)
      )

    vtree.flatMap(vtree => OutWatch.render("#app", vtree)).unsafeRunSync()

    document.getElementById("child").innerHTML shouldBe ""

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    lift {
      document.getElementById("click").dispatchEvent(event)
      unlift(Task(document.getElementById("child").innerHTML shouldBe message))

      //      //dispatch another event
      document.getElementById("click").dispatchEvent(event)
      unlift(Task(document.getElementById("child").innerHTML shouldBe message))

    }.runAsync
  }

  it should "be converted to a generic stream emitter correctly" in {
    import outwatch.dom._

    val messages = createStringHandler().unsafeRunSync()

    val vtree = for {
      stream <- createStringHandler()
    } yield div(id :="click", click(messages) --> stream,
        span(id := "child", child <-- stream)
      )


    vtree.flatMap(vtree => OutWatch.render("#app", vtree)).unsafeRunSync()

    document.getElementById("child").innerHTML shouldBe ""

    val firstMessage = "First"
    messages.observer.onNext(firstMessage)

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

    lift {
      document.getElementById("click").dispatchEvent(event)
      unlift(Task(document.getElementById("child").innerHTML shouldBe firstMessage))

      //dispatch another event
      document.getElementById("click").dispatchEvent(event)
      unlift(Task(document.getElementById("child").innerHTML shouldBe firstMessage))

      val secondMessage = "Second"
      messages.observer.onNext(secondMessage)
      document.getElementById("click").dispatchEvent(event)
      unlift(Task(document.getElementById("child").innerHTML shouldBe secondMessage))
    }.runAsync
  }

  it should "be able to set the value of a text field" in {
    import outwatch.dom._

    val values = PublishSubject[String]

    val vtree = input(id:= "input", outwatch.dom.value <-- values)

    OutWatch.render("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[HTMLInputElement]

    patched.value shouldBe ""

    val value1 = "Hello"
    values.onNext(value1)

    patched.value shouldBe value1

    val value2 = "World"
    values.onNext(value2)

    patched.value shouldBe value2

    values.onNext("")

    patched.value shouldBe ""
  }

  it should "be bindable to a list of children" in {
    import outwatch.dom._


    val state = PublishSubject[Seq[VNode]]


    val vtree = div(
      ul(id:= "list", children <-- state)
    )

    OutWatch.render("#app", vtree).unsafeRunSync()

    val list = document.getElementById("list")

    list.childElementCount shouldBe 0

    val first = "Test"

    state.onNext(Seq(span(first)))

    list.childElementCount shouldBe 1
    list.innerHTML.contains(first) shouldBe true

    val second = "Hello"
    state.onNext(Seq(span(first), span(second)))

    list.childElementCount shouldBe 2
    list.innerHTML.contains(first) shouldBe true
    list.innerHTML.contains(second) shouldBe true

    val third = "World"

    state.onNext(Seq(span(first), span(second), span(third)))

    list.childElementCount shouldBe 3
    list.innerHTML.contains(first) shouldBe true
    list.innerHTML.contains(second) shouldBe true
    list.innerHTML.contains(third) shouldBe true

    state.onNext(Seq(span(first), span(third)))

    list.childElementCount shouldBe 2
    list.innerHTML.contains(first) shouldBe true
    list.innerHTML.contains(third) shouldBe true
  }

  it should "be able to handle two events of the same type" in {
    import outwatch.dom._


    val first = createStringHandler().unsafeRunSync()

    val second = createStringHandler().unsafeRunSync()

    val messages = ("Hello", "World")

    val node = div(
      button(id := "click", click(messages._1) --> first, click(messages._2) --> second),
      span(id:="first",child <-- first),
      span(id:="second",child <-- second)
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    document.getElementById("first").innerHTML shouldBe messages._1
    document.getElementById("second").innerHTML shouldBe messages._2
  }

  it should "be able to be transformed by a function in place" in {
    import outwatch.dom._

    val stream = createHandler[(MouseEvent, Int)]().unsafeRunSync()

    val number = 42

    val mapToTuple = (e: MouseEvent) => (e, number)

    val node = div(
      button(id := "click", click(mapToTuple) --> stream),
      span(id:="num",child <-- stream.map(_._2))
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    document.getElementById("num").innerHTML shouldBe number.toString
  }

  it should "be able to be transformed from strings" in {
    import outwatch.dom._

    val stream = createHandler[Int]().unsafeRunSync()

    val number = 42

    val node = div(
      button(id := "input", inputString(_ => number) --> stream),
      span(id:="num",child <-- stream)
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("input", false, true)

    document.getElementById("input").dispatchEvent(inputEvt)

    Task(document.getElementById("num").innerHTML shouldBe number.toString).runAsync
  }

  it should "be able to toggle attributes with a boolean observer" in {
    import outwatch.dom._
    import outwatch.util.SyntaxSugar._

    val stream = createBoolHandler().unsafeRunSync()

    val someClass = "some-class"

    val node = div(
      button(id := "input", tpe := "checkbox", click(true) --> stream),
      span(id:="toggled", stream ?= (className := someClass))
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    Task(document.getElementById("toggled").classList.contains(someClass) shouldBe true).runAsync
  }

}

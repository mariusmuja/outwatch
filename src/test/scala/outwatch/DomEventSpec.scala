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

    val vtree = createMouseHandler().flatMap { observable =>

      val buttonDisabled = observable.map(_ => true).startWith(Seq(false))

      div(id := "click", click --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()
    document.getElementById("btn").hasAttribute("disabled") shouldBe false

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    Task(document.getElementById("btn").getAttribute("disabled") shouldBe "").runAsync
  }

  it should "be converted to a generic emitter correctly" in {
    import outwatch.dom._

    val message = "ad"

    val vtree = createStringHandler().flatMap { observable =>
      div(id := "click", click(message) --> observable,
        span(id := "child", child <-- observable)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

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

    val vtree = createStringHandler().flatMap { stream =>
      div(id := "click", click(messages) --> stream,
        span(id := "child", child <-- stream)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

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

    val number = 42

    val toTuple = (e: MouseEvent) => (e, number)

    val node = createHandler[(MouseEvent, Int)]().flatMap { stream =>
      div(
        button(id := "click", click.map(toTuple) --> stream),
        span(id := "num", child <-- stream.map(_._2))
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    document.getElementById("num").innerHTML shouldBe number.toString
  }


  it should ".transform should work as expected" in {
    import outwatch.dom._

    val numbers = Observable(1, 2)

    val transformer = (e: Observable[MouseEvent]) => e.concatMap(_ => numbers)

    val node = createHandler[Int]().flatMap { stream =>

      val state = stream.scan(List.empty[Int])((l, s) => l :+ s)

      div(
        button(id := "click", click.transform(transformer) --> stream),
        span(id := "num", children <-- state.map(nums => nums.map(num => span(num.toString))))
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    document.getElementById("num").innerHTML shouldBe "<span>1</span><span>2</span>"
  }

  it should "be able to be transformed from strings" in {
    import outwatch.dom._

    val number = 42
    val node = createHandler[Int]().flatMap { stream =>

      div(
        button(id := "input", inputString(number) --> stream),
        span(id := "num", child <-- stream)
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("input", false, true)

    document.getElementById("input").dispatchEvent(inputEvt)

    Task(document.getElementById("num").innerHTML shouldBe number.toString).runAsync
  }

  it should "be able to toggle attributes with a boolean observer" in {
    import outwatch.dom._
    import outwatch.util.SyntaxSugar._

    val someClass = "some-class"
    val node = createBoolHandler().flatMap { stream =>
      div(
        button(id := "input", tpe := "checkbox", click(true) --> stream),
        span(id := "toggled", stream ?= (className := someClass))
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    Task(document.getElementById("toggled").classList.contains(someClass) shouldBe true).runAsync
  }


  it should "currectly be transformed from latest in observable" in {
    import outwatch.dom._


    val node = createStringHandler().flatMap { submit =>

      val state = submit.scan(List.empty[String])((l, s) => l :+ s)

      createStringHandler().flatMap { stream =>
        div(
          input(id := "input", tpe := "text", inputString --> stream),
          button(id := "submit", click(stream) --> submit),
          ul( id := "items",
            children <-- state.map(items => items.map(it => li(it)))
          )
        )
      }
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputElement = document.getElementById("input").asInstanceOf[HTMLInputElement]
    val submitButton = document.getElementById("submit")

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("input", false, true)

    val clickEvt = document.createEvent("Events")
    clickEvt.initEvent("click", true, true)

    inputElement.value = "item 1"
    inputElement.dispatchEvent(inputEvt)

    inputElement.value = "item 2"
    inputElement.dispatchEvent(inputEvt)

    inputElement.value = "item 3"
    inputElement.dispatchEvent(inputEvt)

    submitButton.dispatchEvent(clickEvt)

    document.getElementById("items").childNodes.length shouldBe 1
  }
}

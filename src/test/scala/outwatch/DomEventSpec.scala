package outwatch

import minitest._
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import org.scalajs.dom.html
import outwatch.dom._
import outwatch.dom.all._
import Deprecated.IgnoreWarnings.initEvent
import monix.eval.Task
import monix.execution.Ack.Continue

import scala.concurrent.duration.FiniteDuration

object DomEventSpec extends TestSuite[Unit] {

  implicit val executionContext = monix.execution.Scheduler.Implicits.global

  def setup(): Unit = {
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  override def tearDown(env: Unit): Unit = {
    document.body.innerHTML = ""
  }

  test("EventStreams should emit and receive events correctly") { _ =>

    val vtree = Handler.mouseEvents.flatMap { observable =>

      val buttonDisabled = observable.map(_ => true).startWith(Seq(false))
      
      div(id := "click", onClick --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()
    assertEquals(document.getElementById("btn").hasAttribute("disabled"), false)

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("btn").getAttribute("disabled"), "")
  }

  test("EventStreams should be converted to a generic emitter correctly") { _ =>

    val message = "ad"

    val vtree = Handler.create[String].flatMap { observable =>
      div(id := "click", onClick(message) --> observable,
        span(id := "child", child <-- observable)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

    assertEquals(document.getElementById("child").innerHTML, "")

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)
  }

  test("EventStreams should be converted to a generic stream emitter correctly") { _ =>

    val messages = Handler.create[String].unsafeRunSync()

    val vtree = Handler.create[String].flatMap { stream =>
      div(id := "click", onClick(messages) --> stream,
        span(id := "child", child <-- stream)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

    assertEquals(document.getElementById("child").innerHTML, "")

    val firstMessage = "First"
    messages.observer.onNext(firstMessage)

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, firstMessage)

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, firstMessage)

    val secondMessage = "Second"
    messages.observer.onNext(secondMessage)

    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, secondMessage)

  }

  test("EventStreams should be able to set the value of a text field") { _ =>
    import outwatch.dom._
    import outwatch.dom.all._

    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)

    OutWatch.render("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]

    assertEquals(patched.value, "")

    val value1 = "Hello"
    values.onNext(value1)

    assertEquals(patched.value, value1)

    val value2 = "World"
    values.onNext(value2)

    assertEquals(patched.value, value2)

    values.onNext("")

    assertEquals(patched.value, "")
  }

  test("EventStreams should preserve user input after setting defaultValue") { _ =>
    val defaultValues = PublishSubject[String]

    val vtree = input(id:= "input", defaultValue <-- defaultValues)
    OutWatch.render("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]
    assertEquals(patched.value, "")

    val value1 = "Hello"
    defaultValues.onNext(value1)
    assertEquals(patched.value, value1)

    val userInput = "user input"
    patched.value = userInput

    defaultValues.onNext("GoodByte")
    assertEquals(patched.value, userInput)
  }

  test("EventStreams should set input value to the same value after user change") { _ =>
    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)
    OutWatch.render("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]
    assertEquals(patched.value, "")

    val value1 = "Hello"
    values.onNext(value1)
    assertEquals(patched.value, value1)

    patched.value = "user input"

    values.onNext(value1)
    assertEquals(patched.value, value1)
  }

  test("EventStreams should be bindable to a list of children") { _ =>

    val state = PublishSubject[Seq[VNode]]

    val vtree = div(
      ul(id:= "list", children <-- state)
    )

    OutWatch.render("#app", vtree).unsafeRunSync()

    val list = document.getElementById("list")

    assertEquals(list.childElementCount, 0)

    val first = "Test"

    state.onNext(Seq(span(first)))

    assertEquals(list.childElementCount, 1)
    assertEquals(list.innerHTML.contains(first), true)

    val second = "Hello"
    state.onNext(Seq(span(first), span(second)))

    assertEquals(list.childElementCount, 2)
    assertEquals(list.innerHTML.contains(first), true)
    assertEquals(list.innerHTML.contains(second), true)

    val third = "World"

    state.onNext(Seq(span(first), span(second), span(third)))

    assertEquals(list.childElementCount, 3)
    assertEquals(list.innerHTML.contains(first), true)
    assertEquals(list.innerHTML.contains(second), true)
    assertEquals(list.innerHTML.contains(third), true)

    state.onNext(Seq(span(first), span(third)))

    assertEquals(list.childElementCount, 2)
    assertEquals(list.innerHTML.contains(first), true)
    assertEquals(list.innerHTML.contains(third), true)
  }

  test("EventStreams should be able to handle two events of the same type") { _ =>

    val first = Handler.create[String].unsafeRunSync()

    val second = Handler.create[String].unsafeRunSync()

    val messages = ("Hello", "World")

    val node = div(
      button(id := "click", onClick(messages._1) --> first, onClick(messages._2) --> second),
      span(id:="first",child <-- first),
      span(id:="second",child <-- second)
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("first").innerHTML, messages._1)
    assertEquals(document.getElementById("second").innerHTML, messages._2)
  }

  test("EventStreams should be able to be transformed by a function in place") { _ =>

    val number = 42

    val toTuple = (e: MouseEvent) => (e, number)

    val node = Handler.create[(MouseEvent, Int)].flatMap { stream =>
      div(
        button(id := "click", onClick.map(toTuple) --> stream),
        span(id := "num", child <-- stream.map(_._2))
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }


  test("EmitterBuilder.transform should work as expected") { _ =>

    val numbers = Observable(1, 2)

    val transformer = (e: Observable[MouseEvent]) => e.concatMap(_ => numbers)

    val node = Handler.create[Int].flatMap { stream =>

      val state = stream.scan(List.empty[Int])((l, s) => l :+ s)

      div(
        button(id := "click", onClick.transform(transformer) --> stream),
        span(id := "num", children <-- state.map(nums => nums.map(num => span(num.toString))))

      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("num").innerHTML, "<span>1</span><span>2</span>")
  }


  test("EventStreams should be able to be transformed from strings") { _ =>

    val number = 42
    val node = Handler.create[Int].flatMap { stream =>
      div(
        button(id := "input", onInputString(number) --> stream),
        span(id:="num",child <-- stream)
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("input", false, true)

    document.getElementById("input").dispatchEvent(inputEvt)

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }


  test("EventStreams should be able to toggle attributes with a boolean observer") { _ =>
    import outwatch.util.SyntaxSugar._

    val someClass = "some-class"
    val node = Handler.create[Boolean].flatMap { stream =>
      div(
        button(id := "input", tpe := "checkbox", onClick(true) --> stream),
        span(id := "toggled", stream ?= (className := someClass))
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    assertEquals(document.getElementById("toggled").classList.contains(someClass), true)
  }


  test("EventStreams should currectly be transformed from latest in observable") { _ =>

    val node = Handler.create[String].flatMap { submit =>

      val state = submit.scan(List.empty[String])((l, s) => l :+ s)

      Handler.create[String].flatMap { stream =>
        div(
          input(id := "input", tpe := "text", onInputString --> stream),
          button(id := "submit", onClick(stream) --> submit),
          ul( id := "items",
            children <-- state.map(items => items.map(it => li(it)))
          )
        )
      }
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val inputElement = document.getElementById("input").asInstanceOf[html.Input]
    val submitButton = document.getElementById("submit")

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("input", false, true)

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    inputElement.value = "item 1"
    inputElement.dispatchEvent(inputEvt)

    inputElement.value = "item 2"
    inputElement.dispatchEvent(inputEvt)

    inputElement.value = "item 3"
    inputElement.dispatchEvent(inputEvt)

    submitButton.dispatchEvent(clickEvt)

    assertEquals(document.getElementById("items").childNodes.length, 1)
  }


  test("Boolean Props should be handled corectly") { _ =>

    val node = Handler.create[Boolean].flatMap { checkValue =>
      div(
        input(id := "checkbox", `type` := "Checkbox", checked <-- checkValue),
        button(id := "on_button", onClick(true) --> checkValue, "On"),
        button(id := "off_button", onClick(false) --> checkValue, "Off")
      )
    }

    OutWatch.render("#app", node).unsafeRunSync()

    val checkbox = document.getElementById("checkbox").asInstanceOf[html.Input]
    val onButton = document.getElementById("on_button")
    val offButton = document.getElementById("off_button")

    assertEquals(checkbox.checked, false)

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    onButton.dispatchEvent(clickEvt)

    assertEquals(checkbox.checked, true)

    offButton.dispatchEvent(clickEvt)

    assertEquals(checkbox.checked, false)
  }

  private val delay = FiniteDuration(20,"ms")

  testAsync("DomWindowEvents and DomDocumentEvents should trigger correctly") { _ =>
    import outwatch.dom._
    import outwatch.dom.all._

    var docClicked = false
    var winClicked = false
    WindowEvents.onClick.subscribe { ev =>
      winClicked = true
      Continue
    }
    DocumentEvents.onClick.subscribe { ev =>
      docClicked = true
      Continue
    }

    val node = div(
      button(id := "input", tpe := "checkbox")
    )

    OutWatch.render("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    (for {
      _ <- Task {
        assertEquals(winClicked, true)
      }.delayExecution(delay)
      _ <- Task {
        assertEquals(docClicked, true)
      }.delayExecution(delay)
    } yield ()).runAsync
  }

}

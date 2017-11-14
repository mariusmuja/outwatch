package outwatch

import minitest._
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLInputElement, MouseEvent}

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
    import outwatch.dom._

    val vtree = createMouseHandler().flatMap { observable =>

      val buttonDisabled = observable.map(_ => true).startWith(Seq(false))

      div(id := "click", click --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()
    assertEquals(document.getElementById("btn").hasAttribute("disabled"), false)

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("btn").getAttribute("disabled"), "")
  }

  test("EventStreams should be converted to a generic emitter correctly") { _ =>
    import outwatch.dom._

    val message = "ad"

    val vtree = createStringHandler().flatMap { observable =>
      div(id := "click", click(message) --> observable,
        span(id := "child", child <-- observable)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

    assertEquals(document.getElementById("child").innerHTML, "")

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)
  }

  test("EventStreams should be converted to a generic stream emitter correctly") { _ =>
    import outwatch.dom._

    val messages = createStringHandler().unsafeRunSync()

    val vtree = createStringHandler().flatMap { stream =>
      div(id := "click", click(messages) --> stream,
        span(id := "child", child <-- stream)
      )
    }

    OutWatch.render("#app", vtree).unsafeRunSync()

    assertEquals(document.getElementById("child").innerHTML, "")

    val firstMessage = "First"
    messages.observer.onNext(firstMessage)

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

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

    val values = PublishSubject[String]

    val vtree = input(id:= "input", outwatch.dom.value <-- values)

    OutWatch.render("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[HTMLInputElement]

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

  test("EventStreams should be bindable to a list of children") { _ =>
    import outwatch.dom._

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

    assertEquals(document.getElementById("first").innerHTML, messages._1)
    assertEquals(document.getElementById("second").innerHTML, messages._2)
  }

  test("EventStreams should be able to be transformed by a function in place") { _ =>
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

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }


  test("EmitterBuilder.transform should work as expected") { _ =>
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

    assertEquals(document.getElementById("num").innerHTML, "<span>1</span><span>2</span>")
  }

  test("EventStreams should be able to be transformed from strings") { _ =>
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

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }

  test("EventStreams should be able to toggle attributes with a boolean observer") { _ =>
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

    assertEquals(document.getElementById("toggled").classList.contains(someClass), true)
  }


  test("EventStreams should currectly be transformed from latest in observable") { _ =>
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

    assertEquals(document.getElementById("items").childNodes.length, 1)
  }
}

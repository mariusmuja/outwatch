package outwatch

import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.{html, _}
import outwatch.Deprecated.IgnoreWarnings.initEvent
import outwatch.dom._
import outwatch.dom.dsl._

object DomEventSpec extends JSDomSuite {

  test("EventStreams should emit and receive events correctly") {

    val vtree = Handler.create[MouseEvent].flatMap { observable =>

      val buttonDisabled = observable.map(_ => true).startWith(Seq(false))
      
      div(id := "click", onClick --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()
    assertEquals(document.getElementById("btn").hasAttribute("disabled"), false)

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("btn").getAttribute("disabled"), "")
  }

  test("EventStreams should be converted to a generic emitter correctly") {

    val message = "ad"

    val vtree = Handler.create[String].flatMap { observable =>
      div(id := "click", onClick(message) --> observable,
        span(id := "child", child <-- observable)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    assertEquals(document.getElementById("child").innerHTML, "")

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)
    assertEquals(document.getElementById("child").innerHTML, message)
  }

  test("EventStreams should be converted to a generic stream emitter correctly") {

    val messages = Handler.create[String].unsafeRunSync()

    val vtree = Handler.create[String].flatMap { stream =>
      div(id := "click", onClick(messages) --> stream,
        span(id := "child", child <-- stream)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

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

  test("EventStreams should be able to set the value of a text field") {
    import outwatch.dom._
    import outwatch.dom.dsl._

    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

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

  test("EventStreams should preserve user input after setting defaultValue") {
    val defaultValues = PublishSubject[String]

    val vtree = input(id:= "input", defaultValue <-- defaultValues)
    OutWatch.renderInto("#app", vtree).unsafeRunSync()

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

  test("EventStreams should set input value to the same value after user change") {
    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)
    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]
    assertEquals(patched.value, "")

    val value1 = "Hello"
    values.onNext(value1)
    assertEquals(patched.value, value1)

    patched.value = "user input"

    values.onNext(value1)
    assertEquals(patched.value, value1)
  }

  test("EventStreams should be bindable to a list of children") {

    val state = PublishSubject[Seq[VNode]]

    val vtree = div(
      ul(id:= "list", children <-- state)
    )

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

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

  test("EventStreams should be able to handle two events of the same type") {

    val first = Handler.create[String].unsafeRunSync()

    val second = Handler.create[String].unsafeRunSync()

    val messages = ("Hello", "World")

    val node = div(
      button(id := "click", onClick(messages._1) --> first, onClick(messages._2) --> second),
      span(id:="first",child <-- first),
      span(id:="second",child <-- second)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("first").innerHTML, messages._1)
    assertEquals(document.getElementById("second").innerHTML, messages._2)
  }

  test("EventStreams should be able to be transformed by a function in place") {

    val number = 42

    val toTuple = (e: MouseEvent) => (e, number)

    val node = Handler.create[(MouseEvent, Int)].flatMap { stream =>
      div(
        button(id := "click", onClick.map(toTuple) --> stream),
        span(id := "num", child <-- stream.map(_._2))
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }


  test("EmitterBuilder.transform should work as expected") {

    val numbers = Observable(1, 2)

    val transformer = (e: Observable[MouseEvent]) => e.concatMap(_ => numbers)

    val node = Handler.create[Int].flatMap { stream =>

      val state = stream.scan(List.empty[Int])((l, s) => l :+ s)

      div(
        button(id := "click", onClick.transform(transformer) --> stream),
        span(id := "num", children <-- state.map(nums => nums.map(num => span(num.toString))))

      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    assertEquals(document.getElementById("num").innerHTML, "<span>1</span><span>2</span>")
  }


  test("EventStreams should be able to be transformed from strings") {

    val number = 42
    val node = Handler.create[Int].flatMap { stream =>
      div(
        input(id := "input", inputString(number) --> stream),
        span(id:="num",child <-- stream)
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("input", false, true)

    document.getElementById("input").dispatchEvent(inputEvt)

    assertEquals(document.getElementById("num").innerHTML, number.toString)
  }


  test("EventStreams should be able to toggle attributes with a boolean observer") {
    import outwatch.util.SyntaxSugar._

    val someClass = "some-class"
    val node = Handler.create[Boolean].flatMap { stream =>
      div(
        button(id := "input", tpe := "checkbox", onClick(true) --> stream),
        span(id := "toggled", stream ?= (className := someClass))
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    assertEquals(document.getElementById("toggled").classList.contains(someClass), true)
  }


  test("EventStreams should currectly be transformed from latest in observable") {

    val node = Handler.create[String].flatMap { submit =>

      val state = submit.scan(List.empty[String])((l, s) => l :+ s)

      Handler.create[String].flatMap { stream =>
        div(
          input(id := "input", tpe := "text", inputString --> stream),
          button(id := "submit", onClick(stream) --> submit),
          ul( id := "items",
            children <-- state.map(items => items.map(it => li(it)))
          )
        )
      }
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

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


  test("Boolean Props should be handled corectly") {

    val node = Handler.create[Boolean].flatMap { checkValue =>
      div(
        input(id := "checkbox", `type` := "Checkbox", checked <-- checkValue),
        button(id := "on_button", onClick(true) --> checkValue, "On"),
        button(id := "off_button", onClick(false) --> checkValue, "Off")
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

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


  test("DomWindowEvents and DomDocumentEvents should trigger correctly") {
    import outwatch.dom.dsl._

    implicit val scheduler = trampolineScheduler

    var docClicked = false
    var winClicked = false
    events.window.onClick(ev => winClicked = true)
    events.document.onClick(ev => docClicked = true)

    val node = div(
      button(id := "input", tpe := "checkbox")
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    assertEquals(winClicked, true)
    assertEquals(docClicked, true)
  }

}

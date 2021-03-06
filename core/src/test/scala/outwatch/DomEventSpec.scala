package outwatch

import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.{html, _}
import outwatch.EventExt._
import outwatch.dom._
import outwatch.dom.dsl.{svg => _, _}

object DomEventSpec extends JSDomSuite {

  test("EventStreams should emit and receive events correctly") {

    val vtree = Handler.create[MouseEvent].flatMap { observable =>

      val buttonDisabled = observable.map(_ => true).startWith(Seq(false))

      div(id := "click", onClick --> observable,
        button(id := "btn", disabled <-- buttonDisabled)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    document.getElementById("btn").hasAttribute("disabled") shouldBe false

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    document.getElementById("btn").hasAttribute("disabled") shouldBe true
    document.getElementById("btn").getAttribute("disabled") shouldBe ""
  }

  test("EventStreams should be converted to a generic emitter correctly") {

    val message = "ad"

    val vtree = Handler.create[String].flatMap { observable =>
      div(id := "click", onClick(message) --> observable,
        span(id := "child", observable)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    document.getElementById("child").innerHTML shouldBe ""

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    document.getElementById("child").innerHTML shouldBe message

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)

    document.getElementById("child").innerHTML shouldBe message


  }

  test("EventStreams should be converted to a generic stream emitter correctly") {

    val messages = Handler.create[String].unsafeRunSync()

    val vtree = Handler.create[String].flatMap { stream =>
      div(id := "click", onClick(messages) --> stream,
        span(id := "child", stream)
      )
    }

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    document.getElementById("child").innerHTML shouldBe ""

    val firstMessage = "First"
    messages.onNext(firstMessage)

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)

    document.getElementById("child").innerHTML shouldBe firstMessage

    //dispatch another event
    document.getElementById("click").dispatchEvent(event)

    document.getElementById("child").innerHTML shouldBe firstMessage

    val secondMessage = "Second"
    messages.onNext(secondMessage)

    document.getElementById("click").dispatchEvent(event)

    document.getElementById("child").innerHTML shouldBe secondMessage
  }

  test("EventStreams should be able to set the value of a text field") {
    import outwatch.dom._
    import outwatch.dom.dsl._

    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]

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

  test("EventStreams should preserve user input after setting defaultValue") {
    val defaultValues = PublishSubject[String]

    val vtree = input(id:= "input", defaultValue <-- defaultValues)
    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]
    patched.value shouldBe ""

    val value1 = "Hello"
    defaultValues.onNext(value1)
    patched.value shouldBe value1

    val userInput = "user input"
    patched.value = userInput

    defaultValues.onNext("GoodByte")
    patched.value shouldBe userInput
  }

  test("EventStreams should set input value to the same value after user change") {
    val values = PublishSubject[String]

    val vtree = input(id:= "input", value <-- values)
    OutWatch.renderInto("#app", vtree).unsafeRunSync()

    val patched = document.getElementById("input").asInstanceOf[html.Input]
    patched.value shouldBe ""

    val value1 = "Hello"
    values.onNext(value1)
    patched.value shouldBe value1

    patched.value = "user input"

    values.onNext(value1)
    patched.value shouldBe value1
  }

  test("EventStreams should be bindable to a list of children") {

    val state = PublishSubject[Seq[VNode]]

    val vtree = div(
      ul(id:= "list", state)
    )

    OutWatch.renderInto("#app", vtree).unsafeRunSync()

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

  test("EventStreams should be able to handle two events of the same type") {

    val first = Handler.create[String].unsafeRunSync()

    val second = Handler.create[String].unsafeRunSync()

    val messages = ("Hello", "World")

    val node = div(
      button(id := "click", onClick(messages._1) --> first, onClick(messages._2) --> second),
      span(id:="first", first),
      span(id:="second", second)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    document.getElementById("first").innerHTML shouldBe messages._1
    document.getElementById("second").innerHTML shouldBe messages._2
  }

  test("EventStreams should be able to be transformed by a function in place") {

    val number = 42

    val toTuple = (e: MouseEvent) => (e, number)

    val node = Handler.create[(MouseEvent, Int)].flatMap { stream =>
      div(
        button(id := "click", onClick.map(toTuple) --> stream),
        span(id := "num", stream.map(_._2))
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)


    document.getElementById("click").dispatchEvent(event)

    document.getElementById("num").innerHTML shouldBe number.toString
  }


  test("EmitterBuilder.transform should work as expected") {

    val numbers = Observable(1, 2)

    val transformer = (e: Observable[MouseEvent]) => e.concatMap(_ => numbers)

    val node = Handler.create[Int].flatMap { stream =>

      val state = stream.scan(List.empty[Int])((l, s) => l :+ s)

      div(
        button(id := "click", onClick.transform(transformer) --> stream),
        span(id := "num", state.map(nums => nums.map(num => span(num.toString))))
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("click").dispatchEvent(event)

    document.getElementById("num").innerHTML shouldBe "<span>1</span><span>2</span>"
  }


  test("EventStreams should be able to be transformed from strings") {

    val number = 42
    val onInputValue = onInput.value
    val node = Handler.create[Int].flatMap { stream =>
      div(
        input(id := "input", onInputValue(number) --> stream),
        span(id:="num", stream)
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("input", false, true)


    document.getElementById("input").dispatchEvent(inputEvt)

    document.getElementById("num").innerHTML shouldBe number.toString
  }

  test("EventStreams can trigger side-effecting functions") {
    var triggeredEventFunction = 0
    var triggeredIntFunction = 0
    var triggeredFunction = 0
    var triggeredFunction2 = 0

    val stream = PublishSubject[String]
    val node = {
      div(
        button(id := "button",
          onClick --> sideEffect{_ => triggeredEventFunction += 1 },
          onClick(1) --> sideEffect[Int](triggeredIntFunction += _),
          onClick --> sideEffect{ triggeredFunction += 1 },
          onUpdate --> sideEffect{(old, current) => triggeredFunction2 += 1},
          stream
        )
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("click", false, true)

    document.getElementById("button").dispatchEvent(inputEvt)
    stream.onNext("woop")
    triggeredEventFunction shouldBe 1
    triggeredIntFunction shouldBe 1
    triggeredFunction shouldBe 1
    triggeredFunction2 shouldBe 1

    document.getElementById("button").dispatchEvent(inputEvt)
    stream.onNext("waap")
    triggeredEventFunction shouldBe 2
    triggeredIntFunction shouldBe 2
    triggeredFunction shouldBe 2
    triggeredFunction2 shouldBe 2
  }

  test("EventStreams should be able to toggle attributes with a boolean observer") {
    import outwatch.dom.SyntaxSugar._

    val someClass = "some-class"
    val node = Handler.create[Boolean].flatMap { stream =>
      div(
        button(id := "input", tpe := "checkbox", onClick(true) --> stream),
        span(id := "toggled", stream ?= (className := someClass))
      )
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    document.getElementById("toggled").classList.contains(someClass) shouldBe true
  }


  test("EventStreams should currectly be transformed from latest in observable") {

    val node = Handler.create[String].flatMap { submit =>

      val state = submit.scan(List.empty[String])((l, s) => l :+ s)

      Handler.create[String].flatMap { stream =>
        div(
          input(id := "input", tpe := "text", onInput.value --> stream),
          button(id := "submit", onClick(stream) --> submit),
          ul( id := "items",
            state.map(items => items.map(it => li(it)))
          )
        )
      }
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputElement = document.getElementById("input").asInstanceOf[html.Input]
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

    checkbox.checked shouldBe false

    val clickEvt = document.createEvent("Events")
    clickEvt.initEvent("click", true, true)

    onButton.dispatchEvent(clickEvt)

    checkbox.checked shouldBe true

    offButton.dispatchEvent(clickEvt)

    checkbox.checked shouldBe false
  }


  test("DomWindowEvents and DomDocumentEvents should trigger correctly") {
    import outwatch.dom.dsl._

    var docClicked = false
    var winClicked = false
    events.window.onClick(ev => winClicked = true)
    events.document.onClick(ev => docClicked = true)

    val node = div(
      button(id := "input", tpe := "checkbox")
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    inputEvt.initEvent("click", true, false)

    document.getElementById("input").dispatchEvent(inputEvt)

    winClicked shouldBe true
    docClicked shouldBe true
  }

  test("EmitterOps should correctly work on events") {

    val node = Handler.create[String].flatMap { submit =>

      for {
        stringStream <- Handler.create[String]
        numberStream <- Handler.create[Double]
        boolStream <- Handler.create[Boolean]
        htmlElementStream <- Handler.create[html.Element]
        svgElementTupleStream <- Handler.create[(svg.Element, svg.Element)]
        elem <- div(
          input(
            id := "input", tpe := "text",

            // target will be html.Input because onSearch is a TypedTargetEvent[html.Input]
            onSearch.target.value --> stringStream,
            onSearch.target.valueAsNumber --> numberStream,
            onSearch.target.checked --> boolStream,

            onClick.target.value --> stringStream,

            onClick.value --> stringStream,
            onClick.valueAsNumber --> numberStream,
            onChange.checked --> boolStream,

            onClick.filter(_ => true).value --> stringStream,

            onInsert.asHtml --> htmlElementStream,
            onUpdate.asSvg --> svgElementTupleStream
          ),
          ul(id := "items")
        )
      } yield elem
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element =document.getElementById("input")
    element shouldNotBe null
  }

  test("DomEvents should correctly be compiled with currentTarget") {

    val stringHandler = Handler.create[String].unsafeRunSync()
    def modifier: VDomModifier = onDrag.value --> stringHandler

    val node = Handler.create[String].flatMap { submit =>

      for {
        stream <- Handler.create[String]
        eventStream <- Handler.create[MouseEvent]
        elem <- div(
          input(
            id := "input", tpe := "text",

            onSearch.target.value --> stream,
            onClick.value --> stream,

            modifier
          ),
          ul(id := "items")
        )
      } yield elem
    }

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("input")
    element shouldNotBe null
  }


  test("Children stream should work for string sequences") {
    val myStrings: Observable[Seq[String]] = Observable(Seq("a", "b"))
    val node = div(id := "strings",
      myStrings
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "ab"
  }
}

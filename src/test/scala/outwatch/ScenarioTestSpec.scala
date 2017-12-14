package outwatch

import monix.eval.Task
import monix.reactive.Observable
import org.scalajs.dom.{html, _}
import outwatch.Deprecated.IgnoreWarnings.initEvent
import outwatch.dom._
import outwatch.dom.dsl._

object ScenarioTestSpec extends JSDomSuite {

  testAsync("A simple counter application should work as intended") {
    val node = for {
      handlePlus <- Handler.create[MouseEvent]
      plusOne = handlePlus.map(_ => 1)

      handleMinus <- Handler.create[MouseEvent]
      minusOne = handleMinus.map(_ => -1)

      count = Observable.merge(plusOne, minusOne).scan(0)(_ + _).startWith(Seq(0))

      div <- div(
        div(
          button(id := "plus", "+", onClick --> handlePlus),
          button(id := "minus", "-", onClick --> handleMinus),
          span(id:="counter",child <-- count)
        )
      )
    } yield div

    val root = document.createElement("div")
    document.body.appendChild(root)

    OutWatch.renderInto(root, node).unsafeRunSync()

    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)

    assertEquals(document.getElementById("counter").innerHTML, 0.toString)

    document.getElementById("minus").dispatchEvent(event)

    (for {
      _ <- Task {
        assertEquals(document.getElementById("counter").innerHTML, (-1).toString)
      }
      _ <- Task.sequence(
        for (i <- 0 to 10) yield {
          for {
            _ <- Task {
              document.getElementById("plus").dispatchEvent(event)
            }
            _ <- Task {
              assertEquals(document.getElementById("counter").innerHTML, i.toString)
            }
          } yield ()
        }
      )

    } yield()).runAsync
  }

  test("A simple name application should work as intended") {
    val greetStart = "Hello ,"

    val node = Handler.create[String].flatMap { nameHandler =>
      div(
        label("Name:"),
        input(id := "input", tpe := "text", inputString --> nameHandler),
        hr(),
        h1(id := "greeting", greetStart, child <-- nameHandler)
      )
    }

    val root = document.createElement("div")
    document.body.appendChild(root)

    OutWatch.renderInto(root, node).unsafeRunSync()


    val evt = document.createEvent("HTMLEvents")
    initEvent(evt)("input", false, true)
    val name = "Luka"

    document.getElementById("input").asInstanceOf[html.Input].value = name
    document.getElementById("input").dispatchEvent(evt)

    assertEquals(document.getElementById("greeting").innerHTML, greetStart + name)

    val name2 = "Peter"

    document.getElementById("input").asInstanceOf[html.Input].value = name2
    document.getElementById("input").dispatchEvent(evt)

    assertEquals(document.getElementById("greeting").innerHTML, greetStart + name2)
  }

  test("A component should be referential transparent") {

    def component() = {
      Handler.create[String].flatMap { handler =>
        div(
          button(onClick("clicked") --> handler),
          div(cls := "label", child <-- handler)
        )
      }
    }

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    val comp = component()

    val component1 = div(component(), component())
    val component2 = div(comp, comp)

    val element1 = document.createElement("div")
    OutWatch.renderInto(element1, component1).unsafeRunSync()

    val element2 = document.createElement("div")
    OutWatch.renderInto(element2, component2).unsafeRunSync()

    element1.getElementsByTagName("button").item(0).dispatchEvent(clickEvt)

    element2.getElementsByTagName("button").item(0).dispatchEvent(clickEvt)

    assertEquals(element1.innerHTML, element2.innerHTML)
  }


//  private val delay = FiniteDuration(20,"ms")

  testAsync("A todo application should work with components") {

    def TodoComponent(title: String, deleteStream: Sink[String]) =
      li(
        span(title),
        button(id:= title, onClick(title) --> deleteStream, "Delete")
      )

    def TextFieldComponent(labelText: String, outputStream: Sink[String]) = for {

      textFieldStream <- Handler.create[String]
      clickStream <- Handler.create[MouseEvent]
      keyStream <- Handler.create[KeyboardEvent]

      buttonDisabled = textFieldStream
        .map(_.length < 2)
        .startWith(Seq(true))

      enterPressed = keyStream
        .filter(_.key == "Enter")

      confirm = Observable.merge(enterPressed, clickStream)
        .withLatestFrom(textFieldStream)((_, input) => input)

      _ <- (outputStream <-- confirm)

      div <- div(
        label(labelText),
        input(id:= "input", tpe := "text", inputString --> textFieldStream, onKeyUp --> keyStream),
        button(id := "submit", onClick --> clickStream, disabled <-- buttonDisabled, "Submit")
      )
    } yield div



    def addToList(todo: String) = {
      (list: Vector[String]) => list :+ todo
    }

    def removeFromList(todo: String) = {
      (list: Vector[String]) => list.filterNot(_ == todo)
    }

    val vtree = for {
      inputHandler <- Handler.create[String]
      deleteHandler <- Handler.create[String]

      adds = inputHandler
        .map(addToList)

      deletes = deleteHandler
        .map(removeFromList)

      state = Observable.merge(adds, deletes)
        .scan(Vector[String]())((state, modify) => modify(state))
        .map(_.map(n => TodoComponent(n, deleteHandler)))
      textFieldComponent = TextFieldComponent("Todo: ", inputHandler)

      div <- div(
        textFieldComponent,
        ul(id:= "list", children <-- state)
      )
    } yield div

    val root = document.createElement("div")
    document.body.appendChild(root)

    OutWatch.renderInto(root, vtree).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("input", false, true)

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    val inputElement = document.getElementById("input").asInstanceOf[html.Input]
    val submitButton = document.getElementById("submit")
    val list = document.getElementById("list")

    assertEquals(list.childElementCount, 0)


    (for {
      todo <- Task {
        val todo = "fold laundry"
        inputElement.value = todo
        inputElement.dispatchEvent(inputEvt)
        submitButton.dispatchEvent(clickEvt)
        todo
      }

      _ <- Task{
        assertEquals(list.childElementCount, 1)
      }.executeWithFork//.delayExecution(delay)

      todo2 <- Task {
        val todo2 = "wash dishes"
        inputElement.value = todo2
        inputElement.dispatchEvent(inputEvt)
        submitButton.dispatchEvent(clickEvt)
        todo2
      }
      _ <- Task{
        assertEquals(list.childElementCount, 2)
      }.executeWithFork//.delayExecution(delay)

      todo3 <- Task {
        val todo3 = "clean windows"
        inputElement.value = todo3
        inputElement.dispatchEvent(inputEvt)
        submitButton.dispatchEvent(clickEvt)
        todo3
      }
      _ <- Task {
        assertEquals(list.childElementCount, 3)
      }.executeWithFork//.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo2).dispatchEvent(clickEvt)
      }

      _ <- Task {
        assertEquals(list.childElementCount, 2)
      }.executeWithFork//.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo3).dispatchEvent(clickEvt)
      }
      _ <- Task {
        assertEquals(list.childElementCount, 1)
      }.executeWithFork//.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo).dispatchEvent(clickEvt)
      }
      _ <- Task {
        assertEquals(list.childElementCount, 0)
      }.executeWithFork//.delayExecution(delay)


    } yield ()).runAsync

  }
}

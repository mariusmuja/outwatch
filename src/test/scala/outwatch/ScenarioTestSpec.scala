package outwatch

import minitest.TestSuite
import monix.eval.Task
import monix.reactive.Observable
import org.scalajs.dom.{html, _}
import outwatch.dom._
import outwatch.dom.helpers.DomUtils

import scala.concurrent.duration.FiniteDuration

import Deprecated.IgnoreWarnings.initEvent

object ScenarioTestSpec extends TestSuite[Unit] {

  implicit val executionContext = monix.execution.Scheduler.Implicits.global

  def setup(): Unit = {
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }

  def tearDown(env: Unit): Unit = {
    document.body.innerHTML = ""
  }

  testAsync("A simple counter application should work as intended") { _ =>
    val node = for {
      handlePlus <- Handler.mouseEvents
      plusOne = handlePlus.map(_ => 1)

      handleMinus <- Handler.mouseEvents
      minusOne = handleMinus.map(_ => -1)

      count = Observable.merge(plusOne, minusOne).scan(0)(_ + _).startWith(Seq(0))

      div <- div(
        div(
          button(id := "plus", "+", click --> handlePlus),
          button(id := "minus", "-", click --> handleMinus),
          span(id := "counter", child <-- count)
        )
      )
    } yield div

    val root = document.createElement("div")
    document.body.appendChild(root)

    DomUtils.render(root, node).unsafeRunSync()

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

  test("A simple name application should work as intended") { _ =>
    val greetStart = "Hello ,"

    val node = Handler.create[String].flatMap { nameHandler =>
      div(
        label("Name:"),
        input(id := "input", inputType := "text", inputString --> nameHandler),
        hr(),
        h1(id := "greeting", greetStart, child <-- nameHandler)
      )
    }

    val root = document.createElement("div")
    document.body.appendChild(root)

    DomUtils.render(root, node).unsafeRunSync()


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

  test("A component should be referential transparent") { _ =>

    def component() = {
      Handler.create[String].flatMap { handler =>
        div(
          button(click("clicked") --> handler),
          div(`class` := "label", child <-- handler)
        )
      }
    }

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    val comp = component()

    val component1 = div(component(), component())
    val component2 = div(comp, comp)

    val element1 = document.createElement("div")
    DomUtils.render(element1, component1).unsafeRunSync()

    val element2 = document.createElement("div")
    DomUtils.render(element2, component2).unsafeRunSync()

    element1.getElementsByTagName("button").item(0).dispatchEvent(clickEvt)

    element2.getElementsByTagName("button").item(0).dispatchEvent(clickEvt)

    assertEquals(element1.innerHTML, element2.innerHTML)
  }


  private val delay = FiniteDuration(20,"ms")

  testAsync("A todo application should work with components") { _ =>

    def TodoComponent(title: String, deleteStream: Sink[String]) =
      li(
        span(title),
        button(id:= title, click(title) --> deleteStream, "Delete")
      )

    def TextFieldComponent(labelText: String, outputStream: Sink[String]) = for {

      textFieldStream <- Handler.create[String]
      clickStream <- Handler.mouseEvents
      keyStream <- Handler.keyboardEvents

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
        input(id:= "input", inputType := "text", inputString --> textFieldStream, keyup --> keyStream),
        button(id := "submit", click --> clickStream, disabled <-- buttonDisabled, "Submit")
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

    DomUtils.render(root, vtree).unsafeRunSync()

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
      }.delayExecution(delay)

      todo2 <- Task {
        val todo2 = "wash dishes"
        inputElement.value = todo2
        inputElement.dispatchEvent(inputEvt)
        submitButton.dispatchEvent(clickEvt)
        todo2
      }
      _ <- Task{
        assertEquals(list.childElementCount, 2)
      }.delayExecution(delay)

      todo3 <- Task {
        val todo3 = "clean windows"
        inputElement.value = todo3
        inputElement.dispatchEvent(inputEvt)
        submitButton.dispatchEvent(clickEvt)
        todo3
      }
      _ <- Task {
        assertEquals(list.childElementCount, 3)
      }.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo2).dispatchEvent(clickEvt)
      }

      _ <- Task {
        assertEquals(list.childElementCount, 2)
      }.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo3).dispatchEvent(clickEvt)
      }
      _ <- Task {
        assertEquals(list.childElementCount, 1)
      }.delayExecution(delay)

      _ <- Task {
        document.getElementById(todo).dispatchEvent(clickEvt)
      }
      _ <- Task {
        assertEquals(list.childElementCount, 0)
      }.delayExecution(delay)


    } yield ()).runAsync

  }
}

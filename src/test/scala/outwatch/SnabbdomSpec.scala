package outwatch

import minitest.SimpleTestSuite
import snabbdom.{DataObject, h, patch}

import scalajs.js
import org.scalajs.dom.document
import org.scalajs.dom.html

import Deprecated.IgnoreWarnings.initEvent


object SnabbdomSpec extends SimpleTestSuite {

  test("The Snabbdom Facade should correctly patch the DOM") {
    val message = "Hello World"
    val vNode = h("span#msg", DataObject(js.Dictionary(), js.Dictionary()), message)

    val node = document.createElement("div")
    document.body.appendChild(node)

    patch(node, vNode)

    assertEquals(document.getElementById("msg").innerHTML, message)

    val newMessage = "Hello Snabbdom!"
    val newNode = h("div#new", DataObject(js.Dictionary(), js.Dictionary()), newMessage)

    patch(vNode, newNode)

    assertEquals(document.getElementById("new").innerHTML, newMessage)
  }

  test("The Snabbdom Facade should correctly patch nodes with keys") {
    import outwatch.dom._

    val clicks = Handler.create[Int](1).unsafeRunSync()
    val nodes = clicks.map { i =>
      div(
        dom.key := s"key-$i",
        span(onClick(if (i == 1) 2 else 1) --> clicks,  s"This is number $i", id := "btn"),
        input(id := "input")
      )
    }

    val node = document.createElement("div")
    node.id = "app"
    document.body.appendChild(node)

    OutWatch.render("#app", div(child <-- nodes)).unsafeRunSync()

    val inputEvt = document.createEvent("HTMLEvents")
    initEvent(inputEvt)("input", false, true)

    val clickEvt = document.createEvent("Events")
    initEvent(clickEvt)("click", true, true)

    def inputElement() = document.getElementById("input").asInstanceOf[html.Input]
    val btn = document.getElementById("btn")

    inputElement().value = "Something"
    inputElement().dispatchEvent(inputEvt)
    btn.dispatchEvent(clickEvt)

    assertEquals(inputElement().value, "")
  }

  test("The Snabbdom Facade should correctly handle boolean attributes") {
    val message = "Hello World"
    val attributes = js.Dictionary[dom.Attr.Value]("bool1" -> true, "bool0" -> false, "string1" -> "true", "string0" -> "false")
    val vNode = h("span#msg", DataObject(attributes, js.Dictionary()), message)

    val node = document.createElement("div")
    document.body.appendChild(node)

    patch(node, vNode)

    val expected = s"""<span id="msg" bool1="" string1="true" string0="false">$message</span>"""
    assertEquals(document.getElementById("msg").outerHTML, expected)
  }
}

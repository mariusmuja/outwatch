package outwatch

import cats.effect.IO
import minitest.TestSuite
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.{Element, document}
import org.scalajs.dom.html
import outwatch.dom.{StringNode, _}
import outwatch.dom.helpers._
import snabbdom.{DataObject, VNodeProxy, hFunction}

import scala.collection.immutable.Seq
import scala.language.reflectiveCalls
import scala.scalajs.js
import scala.scalajs.js.JSON

object OutWatchDomSpec extends TestSuite[Unit]{

  def setup(): Unit = {}

  def tearDown(env: Unit): Unit = {
    document.body.innerHTML = ""
  }

  test("Properties should be separated correctly") { _ =>
    val properties = Seq(
      Attribute("hidden", "true"),
      InsertHook(PublishSubject()),
      UpdateHook(PublishSubject()),
      InsertHook(PublishSubject()),
      DestroyHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject())
    )

    val DomUtils.SeparatedProperties(inserts, prepatch, updates, postpatch, deletes, attributes, keys) = DomUtils.separateProperties(properties)

    assertEquals(inserts.length, 2)
    assertEquals(prepatch.length, 1)
    assertEquals(updates.length,1)
    assertEquals(postpatch.length,1)
    assertEquals(deletes.length, 1)
    assertEquals(attributes.length, 1)
    assertEquals(keys.length, 0)
  }

  test("VDomModifiers should be separated correctly") { _ =>
    val modifiers = Seq(
      Attribute("class", "red"),
      EmptyVDomModifier,
      Emitter("click", _ => ()),
      new StringNode("Test"),
      div().unsafeRunSync(),
      AttributeStreamReceiver("hidden",Observable()),
      CompositeVDomModifier(
        Seq(
          div(),
          Attributes.`class` := "blue",
          Attributes.onClick(1) --> Sink.create[Int](_ => IO.pure(Continue)),
          Attributes.hidden <-- Observable(false)
        )
      )
    )

    val DomUtils.SeparatedModifiers(emitters, receivers, properties, vNodes) = DomUtils.separateModifiers(modifiers)

    assertEquals(emitters.length, 2)
    assertEquals(receivers.length, 2)
    assertEquals(vNodes.length, 3)
    assertEquals(properties.length, 2)

  }

  test("VDomModifiers should be separated correctly with children") { _ =>
    val modifiers = Seq(
      Attribute("class","red"),
      EmptyVDomModifier,
      Emitter("click", _ => ()),
      Emitter("input",  _ => ()),
      AttributeStreamReceiver("hidden",Observable()),
      AttributeStreamReceiver("disabled",Observable()),
      ChildrenStreamReceiver(Observable()),
      Emitter("keyup",  _ => ())
    )

    val DomUtils.SeparatedModifiers(emitters, receivers, properties, children) = DomUtils.separateModifiers(modifiers)

    assertEquals(emitters.length, 3)
    assertEquals(receivers.length, 2)
    assertEquals(properties.length, 1)
    assertEquals(children.length, 1)
  }

  test("VDomModifiers should be separated correctly with children and properties") { _ =>
    val modifiers = Seq(
      Attribute("class","red"),
      EmptyVDomModifier,
      Emitter("click", _ => ()),
      Emitter("input", _ => ()),
      UpdateHook(PublishSubject()),
      AttributeStreamReceiver("hidden",Observable()),
      AttributeStreamReceiver("disabled",Observable()),
      ChildrenStreamReceiver(Observable()),
      Emitter("keyup", _ => ()),
      InsertHook(PublishSubject[Element]),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject())
    )

    val DomUtils.SeparatedModifiers(emitters, receivers, properties, children) = DomUtils.separateModifiers(modifiers)

    val DomUtils.SeparatedProperties(inserts, prepatch, updates, postpatch, deletes, attributes, keys) = DomUtils.separateProperties(properties)

    assertEquals(emitters.map(_.eventType), List("click", "input", "keyup"))
    assertEquals(emitters.length, 3)
    assertEquals(inserts.length, 1)
    assertEquals(prepatch.length, 1)
    assertEquals(updates.length, 1)
    assertEquals(postpatch.length, 1)
    assertEquals(deletes.length, 0)
    assertEquals(attributes.length, 1)
    assertEquals(receivers.length, 2)
    assertEquals(children.length, 1)
    assertEquals(keys.length, 0)

  }

  val fixture = new {
    val proxy = hFunction("div", DataObject(js.Dictionary("class" -> "red", "id" -> "msg"), js.Dictionary()), js.Array(
      hFunction("span", DataObject(js.Dictionary(), js.Dictionary()), js.Array(VNodeProxy.fromString("Hello")))
    ))
  }

  test("VTrees should be constructed correctly") { _ =>


    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    val proxy = fixture.proxy

    assertEquals(JSON.stringify(vtree.map(_.asProxy).unsafeRunSync()), JSON.stringify(proxy))
  }

  test("VTrees should be correctly created with the HyperscriptHelper") { _ =>
    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    assertEquals(JSON.stringify(vtree.map(_.asProxy).unsafeRunSync()), JSON.stringify(fixture.proxy))
  }


  test("VTrees should be correctly patched into the DOM") { _ =>
    val id = "msg"
    val cls = "red"
    val attributes = List(Attribute("class", cls), Attribute("id", id))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)


    val node = document.createElement("div")
    document.body.appendChild(node)

    DomUtils.render(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById(id)

    assertEquals(patchedNode.childElementCount, 1)
    assertEquals(patchedNode.classList.contains(cls), true)
    assertEquals(patchedNode.children(0).innerHTML, message)

  }

  test("VTrees should be replaced if they contain changeables") { _ =>

    def page(num: Int): VNode = {
      val pageNum = Handler.create[Int](num).unsafeRunSync()

      div( id := "page",
        num match {
          case 1 =>
            div(child <-- pageNum)
          case 2 =>
            div(child <-- pageNum)
        }
      )
    }

    val pageHandler =  PublishSubject[Int]

    val vtree = div(
      div(child <-- pageHandler.map(page))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    DomUtils.render(node, vtree).unsafeRunSync()

    pageHandler.onNext(1)

    val domNode = document.getElementById("page")

    assertEquals(domNode.textContent, "1")

    pageHandler.onNext(2)

    assertEquals(domNode.textContent, "2")
  }

  test("The HTML DSL should construct VTrees properly") { _ =>
    import outwatch.dom._

    val vtree = div(cls := "red", id := "msg",
      span("Hello")
    )

    assertEquals(JSON.stringify(vtree.map(_.asProxy).unsafeRunSync()), JSON.stringify(fixture.proxy))
  }

  test("The HTML DSL should construct VTrees with optional children properly") { _ =>
    import outwatch.dom._

    val vtree = div(cls := "red", id := "msg",
      Option(span("Hello")),
      Option.empty[VDomModifier]
    )

    assertEquals(JSON.stringify(vtree.map(_.asProxy).unsafeRunSync()), JSON.stringify(fixture.proxy))

  }

  test("The HTML DSL should construct VTrees with boolean attributes") { _ =>
    import outwatch.dom._

    def boolBuilder(name: String) = new AttributeBuilder[Boolean](name, identity)
    def stringBuilder(name: String) = new AttributeBuilder[Boolean](name, _.toString)
    val vtree = div(
      boolBuilder("a"),
      boolBuilder("b") := true,
      boolBuilder("c") := false,
      stringBuilder("d"),
      stringBuilder("e") := true,
      stringBuilder("f") := false
    )

    val attrs = js.Dictionary[dom.Attr.Value]("a" -> true, "b" -> true, "c" -> false, "d" -> "true", "e" -> "true", "f" -> "false")
    val expected = hFunction("div", DataObject(attrs, js.Dictionary()), js.Array[VNodeProxy]())

    assertEquals(JSON.stringify(vtree.map(_.asProxy).unsafeRunSync()), JSON.stringify(expected))

  }

  test("The HTML DSL should patch into the DOM properly") { _ =>
    import outwatch.dom._

    val message = "Test"
    val vtree = div(cls := "blue", id := "test",
      span(message),
      ul(id := "list",
        li("1"),
        li("2"),
        li("3")
      )
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    DomUtils.render(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById("test")

    assertEquals(patchedNode.childElementCount, 2)
    assertEquals(patchedNode.classList.contains("blue"), true)
    assertEquals(patchedNode.children(0).innerHTML, message)

    assertEquals(document.getElementById("list").childElementCount, 3)
  }

  test("The HTML DSL should change the value of a textfield") { _ =>

    val messages = PublishSubject[String]
    val vtree = div(
      input(outwatch.dom.value <-- messages, id := "input")
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    DomUtils.render(node, vtree).unsafeRunSync()

    val field = document.getElementById("input").asInstanceOf[html.Input]

    assertEquals(field.value, "")

    val message = "Hello"
    messages.onNext(message)
    assertEquals(field.value, message)

    val message2 = "World"
    messages.onNext(message2)

    assertEquals(field.value, message2)

  }

  test("The HTML DSL render child nodes in correct order") { _ =>
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      span("A"),
      child <-- messagesA.map(span(_)),
      span("B"),
      child <-- messagesB.map(span(_))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    assertEquals(node.innerHTML, "<div><span>A</span><span>1</span><span>B</span><span>2</span></div>")
  }

  test("The HTML DSL should render child string-nodes in correct order") { _ =>
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      "A",
      child <-- messagesA,
      "B",
      child <-- messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    assertEquals(node.innerHTML, "<div>A1B2</div>")
  }

  test("The HTML DSL should render child string-nodes in correct order, mixed with children") { _ =>
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val messagesC = PublishSubject[Seq[VNode]]
    val vNode = div(
      "A",
      child <-- messagesA,
      children <-- messagesC,
      "B",
      child <-- messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")
    messagesC.onNext(Seq(div("5"), div("7")))

    assertEquals(node.innerHTML, "<div>A1<div>5</div><div>7</div>B2</div>")
  }

  test("The HTML DSL should update merged nodes children correctly") { _ =>
    val messages = PublishSubject[Seq[VNode]]
    val otherMessages = PublishSubject[Seq[VNode]]
    val vNode = div(children <-- messages)(children <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    otherMessages.onNext(Seq(div("otherMessage")))
    assertEquals(node.children(0).innerHTML, "<div>otherMessage</div>")

    messages.onNext(Seq(div("message")))
    assertEquals(node.children(0).innerHTML, "<div>message</div><div>otherMessage</div>")

    otherMessages.onNext(Seq(div("genus")))
    assertEquals(node.children(0).innerHTML, "<div>message</div><div>genus</div>")
  }

  test("The HTML DSL should update merged nodes separate children correctly") { _ =>
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(child <-- messages)(child <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    assertEquals(node.children(0).innerHTML, "")

    messages.onNext("message")
    assertEquals(node.children(0).innerHTML, "messageotherMessage")

    otherMessages.onNext("genus")
    assertEquals(node.children(0).innerHTML, "messagegenus")
  }

    test("The HTML DSL should update reused vnodes correctly") { _ =>
    val messages = PublishSubject[String]
    val vNode = div(data.ralf := true, child <-- messages)
    val container = div(vNode, vNode)

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, container).unsafeRunSync()

    messages.onNext("message")
    assertEquals(node.children(0).children(0).innerHTML, "message")
    assertEquals(node.children(0).children(1).innerHTML, "message")

    messages.onNext("bumo")
    assertEquals(node.children(0).children(0).innerHTML, "bumo")
    assertEquals(node.children(0).children(1).innerHTML, "bumo")
  }

  test("The HTML DSL should update merged nodes correctly (render reuse)") { _ =>
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNodeTemplate = div(child <-- messages)
    val vNode = vNodeTemplate(child <-- otherMessages)

    val node1 = document.createElement("div")
    document.body.appendChild(node1)
    DomUtils.render(node1, vNodeTemplate).unsafeRunSync()

    val node2 = document.createElement("div")
    document.body.appendChild(node2)
    DomUtils.render(node2, vNode).unsafeRunSync()

    messages.onNext("gurkon")
    otherMessages.onNext("otherMessage")
    assertEquals(node1.children(0).innerHTML, "gurkon")
    assertEquals(node2.children(0).innerHTML, "gurkonotherMessage")

    messages.onNext("message")
    assertEquals(node1.children(0).innerHTML, "message")
    assertEquals(node2.children(0).innerHTML, "messageotherMessage")

    otherMessages.onNext("genus")
    assertEquals(node1.children(0).innerHTML, "message")
    assertEquals(node2.children(0).innerHTML, "messagegenus")
  }

  test("The HTML DSL should update merged node attributes correctly") { _ =>
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(data.noise <-- messages)(data.noise <-- otherMessages)


    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    assertEquals(node.children(0).getAttribute("data-noise"), "otherMessage")

    messages.onNext("message") // should be ignored
    assertEquals(node.children(0).getAttribute("data-noise"), "otherMessage")

    otherMessages.onNext("genus")
    assertEquals(node.children(0).getAttribute("data-noise"), "genus")
  }

  test("The HTML DSL should update merged node styles written with style() correctly") { _ =>
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(style("color") <-- messages)(style("color") <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    messages.onNext("blue") // should be ignored
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    otherMessages.onNext("green")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "green")
  }

  test("The HTML DSL should update merged node styles correctly") { _ =>
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(color <-- messages)(color <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    DomUtils.render(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    messages.onNext("blue") // should be ignored
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    otherMessages.onNext("green")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "green")
  }
}

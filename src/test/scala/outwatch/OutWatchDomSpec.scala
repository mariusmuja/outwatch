package outwatch

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.{document, html}
import outwatch.dom._
import outwatch.dom.all._
import outwatch.dom.helpers._
import snabbdom.{DataObject, hFunction}

import scala.collection.immutable.Seq
import scala.language.reflectiveCalls
import scala.scalajs.js
import scala.scalajs.js.JSON

object OutWatchDomSpec extends JSDomSuite {


  test("Properties should be separated correctly") {
    val properties = Seq(
      Attribute("hidden", "true"),
      InsertHook(PublishSubject()),
      UpdateHook(PublishSubject()),
      InsertHook(PublishSubject()),
      DestroyHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject())
    )

    val SeparatedProperties(att, hooks, keys) = properties.foldRight(SeparatedProperties())((p, sp) => p :: sp)

    assertEquals(hooks.insertHooks.length, 2)
    assertEquals(hooks.prePatchHooks.length, 1)
    assertEquals(hooks.updateHooks.length,1)
    assertEquals(hooks.postPatchHooks.length,1)
    assertEquals(hooks.destroyHooks.length, 1)
    assertEquals(att.attrs.length, 1)
    assertEquals(keys.length, 0)
  }

  test("VDomModifiers should be separated correctly") {
    val modifiers = Seq(
      Attribute("class", "red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      StringModifier("Test"),
      div().unsafeRunSync(),
      AttributeStreamReceiver("hidden",Observable()),
      CompositeModifier(
        Seq(
          div(),
          Attributes.`class` := "blue",
          Attributes.onClick(1) --> Sink.create[Int](_ => IO.pure(Continue)),
          Attributes.hidden <-- Observable(false)
        ).map(_.unsafeRunSync())
      )
    )

    val SeparatedModifiers(properties, emitters, receivers, Children.VNodes(childNodes, streamStatus)) =
      SeparatedModifiers.from(modifiers)

    assertEquals(emitters.emitters.length, 2)
    assertEquals(receivers.length, 2)
    assertEquals(properties.attributes.attrs.length, 2)
    assertEquals(childNodes.length, 3)
    assertEquals(streamStatus.numChild, 0)
    assertEquals(streamStatus.numChildren, 0)
  }

  test("VDomModifiers should be separated correctly with children") {
    val modifiers = Seq(
      Attribute("class", "red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input", _ => Continue),
      AttributeStreamReceiver("hidden", Observable()),
      AttributeStreamReceiver("disabled", Observable()),
      Emitter("keyup", _ => Continue),
      StringModifier("text"),
      div().unsafeRunSync()
    )

    val SeparatedModifiers(properties, emitters, receivers, Children.VNodes(childNodes, streamStatus)) =
      SeparatedModifiers.from(modifiers)

    assertEquals(emitters.emitters.length, 3)
    assertEquals(receivers.length, 2)
    assertEquals(properties.attributes.attrs.length, 1)
    assertEquals(childNodes.length, 2)
    assertEquals(streamStatus.numChild, 0)
    assertEquals(streamStatus.numChildren, 0)
  }

  test("VDomModifiers should be separated correctly with string children") {
    val modifiers: Seq[Modifier] = Seq(
      Attribute("class","red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input", _ => Continue),
      Emitter("keyup", _ => Continue),
      AttributeStreamReceiver("hidden",Observable()),
      AttributeStreamReceiver("disabled",Observable()),
      StringModifier("text"),
      StringVNode("text2")
    )

    val SeparatedModifiers(properties, emitters, receivers, Children.StringModifiers(stringMods)) =
      SeparatedModifiers.from(modifiers)

    assertEquals(emitters.emitters.length, 3)
    assertEquals(receivers.length, 2)
    assertEquals(properties.attributes.attrs.length, 1)
    assertEquals(stringMods.map(_.string).toSet, Set("text", "text2"))
  }

  test("VDomModifiers should be separated correctly with children and properties") {
    val modifiers = Seq(
      Attribute("class","red"),
      EmptyModifier,
      Emitter("click", _ => Continue),
      Emitter("input", _ => Continue),
      UpdateHook(PublishSubject()),
      AttributeStreamReceiver("hidden",Observable()),
      AttributeStreamReceiver("disabled",Observable()),
      ChildrenStreamReceiver(Observable()),
      Emitter("keyup", _ => Continue),
      InsertHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject()),
      StringModifier("text")
    )

    val SeparatedModifiers(properties, emitters, receivers, Children.VNodes(childNodes, streamStatus)) =
      SeparatedModifiers.from(modifiers)

    assertEquals(emitters.emitters.map(_.eventType), List("click", "input", "keyup"))
    assertEquals(properties.hooks.insertHooks.length, 1)
    assertEquals(properties.hooks.prePatchHooks.length, 1)
    assertEquals(properties.hooks.updateHooks.length, 1)
    assertEquals(properties.hooks.postPatchHooks.length, 1)
    assertEquals(properties.hooks.destroyHooks.length, 0)
    assertEquals(properties.attributes.attrs.length, 1)
    assertEquals(receivers.length, 2)
    assertEquals(properties.keys.length, 0)
    assertEquals(childNodes.length, 2)
    assertEquals(streamStatus.numChild, 0)
    assertEquals(streamStatus.numChildren, 1)
  }

  val fixture = new {
    val proxy = hFunction("div", DataObject(js.Dictionary("class" -> "red", "id" -> "msg"), js.Dictionary()), js.Array(
      hFunction("span", DataObject(js.Dictionary(), js.Dictionary()), "Hello")
    ))
  }

  test("VDomModifiers should be run once") {
    val list = new collection.mutable.ArrayBuffer[String]

    val vtree = div(
      IO {
        list += "child1"
        ChildStreamReceiver(Observable())
      },
      IO {
        list += "child2"
        ChildStreamReceiver(Observable())
      },
      IO {
        list += "children1"
        ChildrenStreamReceiver(Observable())
      },
      IO {
        list += "children2"
        ChildrenStreamReceiver(Observable())
      },
      div(
        IO {
          list += "attr1"
          Attribute("attr1", "peter")
        },
        Seq(
          IO {
            list += "attr2"
            Attribute("attr2", "hans")
          }
        )
      )
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    assertEquals(list.isEmpty, true)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    assertEquals(list.toSet, Set("child1", "child2", "children1", "children2", "attr1", "attr2"))
  }

  test("VDomModifiers should provide unique key for child nodes if stream is present") {
    val mods = Seq(
      ChildrenStreamReceiver(Observable()),
      div(id := "1").unsafeRunSync(),
      div(id := "2").unsafeRunSync()
      // div().unsafeRunSync(), div().unsafeRunSync() //TODO: this should also work, but key is derived from hashCode of VTree (which in this case is equal)
    )

    val modifiers =  SeparatedModifiers.from(mods)
    val Children.VNodes(childNodes, streamStatus) = modifiers.children

    assertEquals(childNodes.size, 3)
    assertEquals(streamStatus.numChild, 0)
    assertEquals(streamStatus.numChildren, 1)

    val proxy = modifiers.toSnabbdom("div")
    assertEquals(proxy.key.isDefined, true)

    assertEquals(proxy.children.get.length, 2)

    val key1 = proxy.children.get(0).key
    val key2 = proxy.children.get(1).key

    assertEquals(key1.isDefined, true)
    assertEquals(key2.isDefined, true)
    assert(key1.get != key2.get)
  }

  test("VDomModifiers should keep existing key for child nodes") {
    val mods = Seq(
      Key(1234),
      ChildrenStreamReceiver(Observable()),
      div()(IO.pure(Key(5678))).unsafeRunSync()
    )

    val modifiers =  SeparatedModifiers.from(mods)
    val Children.VNodes(childNodes, streamStatus) = modifiers.children

    assertEquals(childNodes.size, 2)
    assertEquals(streamStatus.numChild, 0)
    assertEquals(streamStatus.numChildren, 1)

    val proxy = modifiers.toSnabbdom("div")
    assertEquals(proxy.key.toOption, Some(1234))

    assertEquals(proxy.children.get(0).key.toOption, Some(5678))
  }


  test("VTrees should be constructed correctly") {


    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    val proxy = fixture.proxy

    assertEquals(JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()), JSON.stringify(proxy))
  }

  test("VTrees should be correctly created with the HyperscriptHelper") {
    val attributes = List(Attribute("class", "red"), Attribute("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    assertEquals(JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()), JSON.stringify(fixture.proxy))
  }


  test("VTrees should run its modifiers once!" ) {
    val stringHandler = Handler.create[String]().unsafeRunSync()
    var ioCounter = 0
    var handlerCounter = 0
    stringHandler { _ =>
      handlerCounter += 1
    }

    val vtree = div(
      div(
        IO {
          ioCounter += 1
          Attribute("hans", "")
        }
      ),
      child <-- stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    assertEquals(ioCounter, 0)
    assertEquals(handlerCounter, 0)
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    assertEquals(ioCounter, 1)
    assertEquals(handlerCounter, 0)
    stringHandler.observer.onNext("pups")
    assertEquals(ioCounter, 1)
    assertEquals(handlerCounter, 1)
  }

  test("VTrees should run its modifiers once in CompositeModifier!") {
    val stringHandler = Handler.create[String]().unsafeRunSync()
    var ioCounter = 0
    var handlerCounter = 0
    stringHandler { _ =>
      handlerCounter += 1
    }

    val vtree = div(
      div(Seq(
        IO {
          ioCounter += 1
          Attribute("hans", "")
        }
      )),
      child <-- stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    assertEquals(ioCounter, 0)
    assertEquals(handlerCounter, 0)
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    assertEquals(ioCounter, 1)
    assertEquals(handlerCounter, 0)
    stringHandler.observer.onNext("pups")
    assertEquals(ioCounter, 1)
    assertEquals(handlerCounter, 1)
  }

  test("VTrees should be correctly patched into the DOM") {
    val id = "msg"
    val cls = "red"
    val attributes = List(Attribute("class", cls), Attribute("id", id))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)


    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById(id)

    assertEquals(patchedNode.childElementCount, 1)
    assertEquals(patchedNode.classList.contains(cls), true)
    assertEquals(patchedNode.children(0).innerHTML, message)

  }

  test("VTrees should be replaced if they contain changeables") {

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

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    pageHandler.onNext(1)

    val domNode = document.getElementById("page")

    assertEquals(domNode.textContent, "1")

    pageHandler.onNext(2)

    assertEquals(domNode.textContent, "2")
  }

  test("The HTML DSL should construct VTrees properly") {
    import outwatch.dom._
    import outwatch.dom.all._

    val vtree = div(cls := "red", id := "msg",
      span("Hello")
    )

    assertEquals(JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()), JSON.stringify(fixture.proxy))
  }

  test("The HTML DSL should construct VTrees with optional children properly") {
    import outwatch.dom._
    import outwatch.dom.all._

    val vtree = div(cls := "red", id := "msg",
      Option(span("Hello")),
      Option.empty[VDomModifier]
    )

    assertEquals(JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()), JSON.stringify(fixture.proxy))

  }

  test("The HTML DSL should construct VTrees with boolean attributes") {
    import outwatch.dom.all._

    def boolBuilder(name: String) = new AttributeBuilder[Boolean](name, identity[Boolean])
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
    val expected = hFunction("div", DataObject(attrs, js.Dictionary()))

    assertEquals(JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()), JSON.stringify(expected))

  }

  test("The HTML DSL should patch into the DOM properly") {
    import outwatch.dom._
    import outwatch.dom.all._

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

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById("test")

    assertEquals(patchedNode.childElementCount, 2)
    assertEquals(patchedNode.classList.contains("blue"), true)
    assertEquals(patchedNode.children(0).innerHTML, message)

    assertEquals(document.getElementById("list").childElementCount, 3)
  }

  test("The HTML DSL should change the value of a textfield") {

    val messages = PublishSubject[String]
    val vtree = div(
      input(value <-- messages, id := "input")
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val field = document.getElementById("input").asInstanceOf[html.Input]

    assertEquals(field.value, "")

    val message = "Hello"
    messages.onNext(message)
    assertEquals(field.value, message)

    val message2 = "World"
    messages.onNext(message2)

    assertEquals(field.value, message2)

  }

  test("The HTML DSL render child nodes in correct order") {
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
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    assertEquals(node.innerHTML, "<div><span>A</span><span>1</span><span>B</span><span>2</span></div>")
  }

  test("The HTML DSL should render child string-nodes in correct order") {
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
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    assertEquals(node.innerHTML, "<div>A1B2</div>")
  }

  test("The HTML DSL should render child string-nodes in correct order, mixed with children") {
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
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")
    messagesC.onNext(Seq(div("5"), div("7")))

    assertEquals(node.innerHTML, "<div>A1<div>5</div><div>7</div>B2</div>")
  }

  test("The HTML DSL should update merged nodes children correctly") {
    val messages = PublishSubject[Seq[VNode]]
    val otherMessages = PublishSubject[Seq[VNode]]
    val vNode = div(children <-- messages)(children <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext(Seq(div("otherMessage")))
    assertEquals(node.children(0).innerHTML, "<div>otherMessage</div>")

    messages.onNext(Seq(div("message")))
    assertEquals(node.children(0).innerHTML, "<div>message</div><div>otherMessage</div>")

    otherMessages.onNext(Seq(div("genus")))
    assertEquals(node.children(0).innerHTML, "<div>message</div><div>genus</div>")
  }

  test("The HTML DSL should update merged nodes separate children correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(child <-- messages)(child <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    assertEquals(node.children(0).innerHTML, "")

    messages.onNext("message")
    assertEquals(node.children(0).innerHTML, "messageotherMessage")

    otherMessages.onNext("genus")
    assertEquals(node.children(0).innerHTML, "messagegenus")
  }

  test("The HTML DSL should update reused vnodes correctly") {
    val messages = PublishSubject[String]
    val vNode = div(data.ralf := true, child <-- messages)
    val container = div(vNode, vNode)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, container).unsafeRunSync()

    messages.onNext("message")
    assertEquals(node.children(0).children(0).innerHTML, "message")
    assertEquals(node.children(0).children(1).innerHTML, "message")

    messages.onNext("bumo")
    assertEquals(node.children(0).children(0).innerHTML, "bumo")
    assertEquals(node.children(0).children(1).innerHTML, "bumo")
  }

  test("The HTML DSL should update merged nodes correctly (render reuse)") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNodeTemplate = div(child <-- messages)
    val vNode = vNodeTemplate(child <-- otherMessages)

    val node1 = document.createElement("div")
    document.body.appendChild(node1)
    OutWatch.renderInto(node1, vNodeTemplate).unsafeRunSync()

    val node2 = document.createElement("div")
    document.body.appendChild(node2)
    OutWatch.renderInto(node2, vNode).unsafeRunSync()

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

  test("The HTML DSL should update merged node attributes correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(data.noise <-- messages)(data.noise <-- otherMessages)


    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    assertEquals(node.children(0).getAttribute("data-noise"), "otherMessage")

    messages.onNext("message") // should be ignored
    assertEquals(node.children(0).getAttribute("data-noise"), "otherMessage")

    otherMessages.onNext("genus")
    assertEquals(node.children(0).getAttribute("data-noise"), "genus")
  }

  test("The HTML DSL should update merged node styles written with style() correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(style("color") <-- messages)(style("color") <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    messages.onNext("blue") // should be ignored
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    otherMessages.onNext("green")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "green")
  }

  test("The HTML DSL should update merged node styles correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(color <-- messages)(color <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    messages.onNext("blue") // should be ignored
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "red")

    otherMessages.onNext("green")
    assertEquals(node.children(0).asInstanceOf[html.Element].style.color, "green")
  }

  test("The HTML DSL should render composite VNodes properly") {
    val items = Seq("one", "two", "three")
    val vNode = div(items.map(item => span(item)))

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    assertEquals(node.innerHTML, "<div><span>one</span><span>two</span><span>three</span></div>")
  }

  test("The HTML DSL should render nodes with only attribute receivers properly") {
    val classes = PublishSubject[String]
    val vNode = button( className <-- classes, "Submit")

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    classes.onNext("active")

    assertEquals(node.innerHTML, """<button class="active">Submit</button>""")
  }


  test("The HTML DSL should work with custom tags") {

    val vNode = div(tag("main")())

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderReplace(node, vNode).unsafeRunSync()

    assertEquals(node.innerHTML, "<main></main>")
  }


  test("The HTML DSL should work with un-assigned booleans attributes and props") {

    val vNode = option(selected, disabled)

    val node = document.createElement("option").asInstanceOf[html.Option]
    document.body.appendChild(node)

    assertEquals(node.selected, false)
    assertEquals(node.disabled, false)

    OutWatch.renderReplace(node, vNode).unsafeRunSync()

    assertEquals(node.selected, true)
    assertEquals(node.disabled, true)
  }

  test("class attributes should be merged") {
    val vNode = button(
      classToggle := ("class1" -> true),
      classToggle := ("class2" -> true),
      "Submit"
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vNode).unsafeRunSync()

    assertEquals(node.innerHTML, """<button class="class1 class2">Submit</button>""")

  }

}

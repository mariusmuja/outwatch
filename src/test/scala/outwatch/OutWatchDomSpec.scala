package outwatch

import monix.execution.Ack.Continue
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.window.localStorage
import org.scalajs.dom.{document, html}
import outwatch.Deprecated.IgnoreWarnings.initEvent
import outwatch.dom._
import outwatch.dom.dsl._
import outwatch.dom.helpers._
import snabbdom.{DataObject, hFunction}

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.scalajs.js
import scala.scalajs.js.JSON

object OutWatchDomSpec extends JSDomSuite {

  test("Properties should be separated correctly") {
    val modifiers = Seq(
      BasicAttr("hidden", "true"),
      InsertHook(PublishSubject()),
      UpdateHook(PublishSubject()),
      InsertHook(PublishSubject()),
      DestroyHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject())
    )

    val VNodeState(mods, streams) = VNodeState.from(modifiers)

    mods.hooks.insertHooks.length shouldBe 2
    mods.hooks.prePatchHooks.length shouldBe 1
    mods.hooks.updateHooks.length shouldBe 1
    mods.hooks.postPatchHooks.length shouldBe 1
    mods.hooks.destroyHooks.length shouldBe 1
    mods.attributes.attrs.size shouldBe 1
    mods.keyOption shouldBe None
    streams shouldBe Observable.empty
  }

  test("VDomModifiers should be separated correctly") {
    import dsl.attributes

    val modifiers = Seq(
      BasicAttr("class", "red"),
      EmptyModifier,
      Emitter("click", _ => ()),
      StringVNode("Test"),
      div().unsafeRunSync(),
      ModifierStream(Observable()),
      CompositeModifier(
        Seq(
          div(),
          attributes.`class` := "blue",
          attributes.onClick(1) --> Sink.create[Int](_ => Continue).unsafeRunSync(),
          attributes.hidden <-- Observable(false)
        ).map(_.unsafeRunSync())
      )
    )

    val VNodeState(mods, streams) = VNodeState.from(modifiers)

    mods.emitters.emitters.length shouldBe 2
    mods.attributes.attrs.size shouldBe 1
    mods.nodes.length shouldBe 3
    streams shouldNotBe Observable.empty
  }

  test("VDomModifiers should be separated correctly with children") {
    val modifiers = Seq(
      BasicAttr("class", "red"),
      EmptyModifier,
      Emitter("click", _ => ()),
      Emitter("input", _ => ()),
      ModifierStream(Observable()),
      ModifierStream(Observable()),
      Emitter("keyup", _ => ()),
      StringVNode("text"),
      div().unsafeRunSync()
    )

    val VNodeState(mods, streams) = VNodeState.from(modifiers)

    mods.emitters.emitters.length shouldBe 3
    mods.attributes.attrs.size shouldBe 1
    mods.nodes.length shouldBe 2
    streams shouldNotBe Observable.empty
  }

  test("VDomModifiers should be separated correctly with string children") {
    val modifiers: Seq[Modifier] = Seq(
      BasicAttr("class","red"),
      EmptyModifier,
      Emitter("click", _ => ()),
      Emitter("input", _ => ()),
      Emitter("keyup", _ => ()),
      ModifierStream(Observable()),
      ModifierStream(Observable()),
      StringVNode("text"),
      StringVNode("text2"),
      Key("123")
    )

    val VNodeState(mods, streams) = VNodeState.from(modifiers)

    mods.emitters.emitters.length shouldBe 3
    mods.attributes.attrs.size shouldBe 1
    mods.nodes.collect{ case StringVNode(s) => s}.toSet shouldBe Set("text", "text2")
    streams shouldNotBe Observable.empty
    mods.keyOption shouldBe Some(Key("123"))
  }

  test("VDomModifiers should be separated correctly with children and properties") {
    val modifiers = Seq(
      BasicAttr("class","red"),
      EmptyModifier,
      Emitter("click", _ => ()),
      Emitter("input", _ => ()),
      UpdateHook(PublishSubject()),
      ModifierStream(Observable()),
      ModifierStream(Observable()),
      ModifierStream(Observable()),
      Emitter("keyup", _ => ()),
      InsertHook(PublishSubject()),
      PrePatchHook(PublishSubject()),
      PostPatchHook(PublishSubject()),
      StringVNode("text")
    )

    val VNodeState(mods, streams) = VNodeState.from(modifiers)

    mods.emitters.emitters.map(_.eventType).toList shouldBe List("click", "input", "keyup")
    mods.hooks.insertHooks.length shouldBe 1
    mods.hooks.prePatchHooks.length shouldBe 1
    mods.hooks.updateHooks.length shouldBe 1
    mods.hooks.postPatchHooks.length shouldBe 1
    mods.hooks.destroyHooks.length shouldBe 0
    mods.attributes.attrs.size shouldBe 1
    mods.keyOption shouldNotBe None // because ModifierStreams present
//    nodes.length shouldBe 2
    streams shouldNotBe Observable.empty
  }

  test("VDomModifiers should be run once") {
    val list = new mutable.ListBuffer[String]

    val vtree = div(
      IO {
        list += "child1"
        ModifierStream(Observable(div()))
      },
      IO {
        list += "child2"
        ModifierStream(Observable())
      },
      IO {
        list += "children1"
        ModifierStream(Observable())
      },
      IO {
        list += "children2"
        ModifierStream(Observable())
      },
      div(
        IO {
          list += "attr1"
          BasicAttr("attr1", "peter")
        },
        Seq(
          IO {
            list += "attr2"
            BasicAttr("attr2", "hans")
          }
        )
      )
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    list.isEmpty shouldBe true

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    list.toSeq shouldBe Seq("child1", "child2", "children1", "children2", "attr1", "attr2")
  }

  test("VDomModifiers should provide unique key for child nodes if stream is present") {
    val mods = Seq[Modifier](
      ModifierStream(Observable()),
      div(id := "1").unsafeRunSync(),
      div(id := "2").unsafeRunSync()
      // div().unsafeRunSync(), div().unsafeRunSync() //TODO: this should also work, but key is derived from hashCode of VTree (which in this case is equal)
    )

    val state =  VNodeState.from(mods)
    val nodes = state.initial.nodes

    nodes.length shouldBe 2
    state.stream shouldNotBe Observable.empty

    val proxy = state.toSnabbdom("div")
    proxy.key.isDefined shouldBe true

    proxy.children.get.length shouldBe 2

    val key1 = proxy.children.get(0).key
    val key2 = proxy.children.get(1).key

    key1.isDefined shouldBe true
    key2.isDefined shouldBe true
    key1.get shouldNotBe key2.get
  }

  test("VDomModifiers should keep existing key for child nodes") {
    val mods = Seq(
      Key(1234),
      ModifierStream(Observable()),
      div()(IO.pure(Key(5678))).unsafeRunSync()
    )

    val state = VNodeState.from(mods)
    val nodes = state.initial.nodes

    nodes.length shouldBe 1
    state.stream shouldNotBe Observable.empty

    val proxy = state.toSnabbdom("div")
    proxy.key.toOption shouldBe Some(1234)

    proxy.children.get(0).key.toOption shouldBe Some(5678)
  }
//

  test("VTrees should be constructed correctly") {

    val attributes = List(BasicAttr("class", "red"), BasicAttr("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    val proxy = fixture.proxy

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(proxy)
  }

  test("VTrees should be correctly created with the HyperscriptHelper") {
    val attributes = List(BasicAttr("class", "red"), BasicAttr("id", "msg"))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)
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
          BasicAttr("hans", "")
        }
      ),
      stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    ioCounter shouldBe 0
    handlerCounter shouldBe 0
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    ioCounter shouldBe 1
    handlerCounter shouldBe 0
    stringHandler.observer.onNext("pups")
    ioCounter shouldBe 1
    handlerCounter shouldBe 1
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
          BasicAttr("hans", "")
        }
      )),
      stringHandler
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    ioCounter shouldBe 0
    handlerCounter shouldBe 0
    OutWatch.renderInto(node, vtree).unsafeRunSync()
    ioCounter shouldBe 1
    handlerCounter shouldBe 0
    stringHandler.observer.onNext("pups")
    ioCounter shouldBe 1
    handlerCounter shouldBe 1
  }

  test("VTrees should be correctly patched into the DOM") {
    val id = "msg"
    val cls = "red"
    val attributes = List(BasicAttr("class", cls), BasicAttr("id", id))
    val message = "Hello"
    val child = span(message)
    val vtree = div(IO.pure(attributes.head), IO.pure(attributes(1)), child)


    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    val patchedNode = document.getElementById(id)

    patchedNode.childElementCount shouldBe 1
    patchedNode.classList.contains(cls) shouldBe true
    patchedNode.children(0).innerHTML shouldBe message
  }

  test("VTrees should be replaced if they contain changeables") {

    def page(num: Int): VNode = {
      val pageNum = Handler.create[Int](num).unsafeRunSync()

      div( id := "page",
        num match {
          case 1 =>
            div(pageNum)
          case 2 =>
            div(pageNum)
        }
      )
    }

    val pageHandler =  PublishSubject[Int]

    val vtree = div(
      div(pageHandler.map(page))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    pageHandler.onNext(1)

    def domNode = document.getElementById("page")

    domNode.textContent shouldBe "1"

    pageHandler.onNext(2)

    domNode.textContent shouldBe "2"
  }


  val fixture = new {
    val proxy = hFunction("div", DataObject(js.Dictionary("class" -> "red", "id" -> "msg"), js.Dictionary()), js.Array(
      hFunction("span", DataObject(js.Dictionary(), js.Dictionary()), "Hello")
    ))
  }

  // TODO: fix fragile test, depende on key order
  test("The HTML DSL should construct VTrees properly") {
    import outwatch.dom._
    import outwatch.dom.dsl._

    val vtree = div(cls := "red", id := "msg",
      span("Hello")
    )

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)
  }

  // TODO: fix fragile test, depende on key order
  test("The HTML DSL should construct VTrees with optional children properly") {
    import outwatch.dom._
    import outwatch.dom.dsl._

    val vtree = div(cls := "red", id := "msg",
      Option(span("Hello")),
      Option.empty[VDomModifier]
    )

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(fixture.proxy)
  }

  // TODO: fix fragile test, depende on key order
  test("The HTML DSL should construct VTrees with boolean attributes") {
    import outwatch.dom.dsl._

    def boolBuilder(name: String) = new BasicAttrBuilder[Boolean](name, identity)
    def stringBuilder(name: String) = new BasicAttrBuilder[Boolean](name, _.toString)
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

    JSON.stringify(vtree.map(_.toSnabbdom).unsafeRunSync()) shouldBe JSON.stringify(expected)
  }

  test("The HTML DSL should patch into the DOM properly") {
    import outwatch.dom._
    import outwatch.dom.dsl._

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

    patchedNode.childElementCount shouldBe 2
    patchedNode.classList.contains("blue") shouldBe true
    patchedNode.children(0).innerHTML shouldBe message

    document.getElementById("list").childElementCount shouldBe 3
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

    field.value shouldBe ""

    val message = "Hello"
    messages.onNext(message)

    field.value shouldBe message

    val message2 = "World"
    messages.onNext(message2)

    field.value shouldBe message2
  }

  test("The HTML DSL render child nodes in correct order") {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      span("A"),
      messagesA.map(span(_)),
      span("B"),
      messagesB.map(span(_))
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesA.onNext("1")
    messagesB.onNext("2")

    node.innerHTML shouldBe "<div><span>A</span><span>1</span><span>B</span><span>2</span></div>"
  }

  test("The HTML DSL should render child string-nodes in correct order") {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      "A",
      messagesA,
      "B",
      messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div>AB</div>"

    messagesA.onNext("1")
    node.innerHTML shouldBe "<div>A1B</div>"

    messagesB.onNext("2")
    node.innerHTML shouldBe "<div>A1B2</div>"
  }

  test("The HTML DSL should render multiple child string-nodes correctly") {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val vNode = div(
      "A",
      messagesA,
      "B",
      messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    messagesB.onNext("2")
    node.innerHTML shouldBe "<div>AB2</div>"

    messagesA.onNext("1")
    node.innerHTML shouldBe "<div>A1B2</div>"
  }

  test("The HTML DSL should render child string-nodes in correct order, mixed with children") {
    val messagesA = PublishSubject[String]
    val messagesB = PublishSubject[String]
    val messagesC = PublishSubject[Seq[VNode]]
    val messagesD = PublishSubject[Seq[Int]]
    val vNode = div(
      "A",
      child <-- messagesA,
      messagesC,
      child <-- messagesD,
      "B",
      messagesB
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div>AB</div>"

    messagesA.onNext("1")
    node.innerHTML shouldBe "<div>A1B</div>"

    messagesB.onNext("2")
    node.innerHTML shouldBe "<div>A1B2</div>"

    messagesC.onNext(Seq(div("5"), div("7")))
    node.innerHTML shouldBe "<div>A1<div>5</div><div>7</div>B2</div>"

    messagesD.onNext(Seq(1, 2, 3))
    node.innerHTML shouldBe "<div>A1<div>5</div><div>7</div>123B2</div>"
  }

  test("The HTML DSL should update merged nodes children correctly") {
    val messages = PublishSubject[Seq[VNode]]
    val otherMessages = PublishSubject[Seq[VNode]]
    val vNode = div(messages)(otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext(Seq(div("otherMessage")))
    node.children(0).innerHTML shouldBe "<div>otherMessage</div>"

    messages.onNext(Seq(div("message")))
    node.children(0).innerHTML shouldBe "<div>message</div><div>otherMessage</div>"

    otherMessages.onNext(Seq(div("genus")))
    node.children(0).innerHTML shouldBe "<div>message</div><div>genus</div>"
  }

  test("The HTML DSL should update merged nodes separate children correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(messages)(otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.children(0).innerHTML shouldBe ""

    otherMessages.onNext("otherMessage")
    node.children(0).innerHTML shouldBe "otherMessage"

    messages.onNext("message")
    node.children(0).innerHTML shouldBe "messageotherMessage"

    otherMessages.onNext("genus")
    node.children(0).innerHTML shouldBe "messagegenus"
  }

  test("The HTML DSL partially render component even if parts not present") {
    val messagesColor = PublishSubject[String]
    val messagesBgColor = PublishSubject[String]
    val childString = PublishSubject[String]

    val vNode = div( id := "inner",
      color <-- messagesColor,
      backgroundColor <-- messagesBgColor,
      childString
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()
    val inner = document.getElementById("inner").asInstanceOf[html.Div]

    inner.innerHTML shouldBe ""
    inner.style.color shouldBe ""
    inner.style.backgroundColor shouldBe ""

    childString.onNext("fish")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe ""
    inner.style.backgroundColor shouldBe ""

    messagesColor.onNext("red")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
    inner.style.backgroundColor shouldBe ""

    messagesBgColor.onNext("blue")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
    inner.style.backgroundColor shouldBe "blue"
  }

  test("The HTML DSL partially render component even if parts not present2") {
    val messagesColor = PublishSubject[String]
    val childString = PublishSubject[String]

    val vNode = div( id := "inner",
      color <-- messagesColor,
      childString
    )

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()
    val inner = document.getElementById("inner").asInstanceOf[html.Div]

    inner.innerHTML shouldBe ""
    inner.style.color shouldBe ""

    childString.onNext("fish")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe ""

    messagesColor.onNext("red")
    inner.innerHTML shouldBe "fish"
    inner.style.color shouldBe "red"
  }

  test("The HTML DSL should update reused vnodes correctly") {
    val messages = PublishSubject[String]
    val vNode = div(data.ralf := true, messages)
    val container = div(vNode, vNode)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, container).unsafeRunSync()

    messages.onNext("message")
    node.children(0).children(0).innerHTML shouldBe "message"
    node.children(0).children(1).innerHTML shouldBe "message"

    messages.onNext("bumo")
    node.children(0).children(0).innerHTML shouldBe "bumo"
    node.children(0).children(1).innerHTML shouldBe "bumo"
  }

  test("The HTML DSL should update merged nodes correctly (render reuse)") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNodeTemplate = div(messages)
    val vNode = vNodeTemplate(otherMessages)

    val node1 = document.createElement("div")
    document.body.appendChild(node1)
    OutWatch.renderInto(node1, vNodeTemplate).unsafeRunSync()

    val node2 = document.createElement("div")
    document.body.appendChild(node2)
    OutWatch.renderInto(node2, vNode).unsafeRunSync()

    messages.onNext("gurkon")
    otherMessages.onNext("otherMessage")
    node1.children(0).innerHTML shouldBe "gurkon"
    node2.children(0).innerHTML shouldBe "gurkonotherMessage"

    messages.onNext("message")
    node1.children(0).innerHTML shouldBe "message"
    node2.children(0).innerHTML shouldBe "messageotherMessage"

    otherMessages.onNext("genus")
    node1.children(0).innerHTML shouldBe "message"
    node2.children(0).innerHTML shouldBe "messagegenus"
  }

  test("The HTML DSL should update merged node attributes correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(data.noise <-- messages)(data.noise <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("otherMessage")
    node.children(0).getAttribute("data-noise") shouldBe "otherMessage"

    messages.onNext("message")
    node.children(0).getAttribute("data-noise") shouldBe "otherMessage"

    otherMessages.onNext("genus")
    node.children(0).getAttribute("data-noise") shouldBe "genus"
  }

  test("The HTML DSL should update merged node styles written with style() correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(style("color") <-- messages)(style("color") <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    messages.onNext("blue")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    otherMessages.onNext("green")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "green"
  }

  test("The HTML DSL should update merged node styles correctly") {
    val messages = PublishSubject[String]
    val otherMessages = PublishSubject[String]
    val vNode = div(color <-- messages)(color <-- otherMessages)

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    otherMessages.onNext("red")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    messages.onNext("blue")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "red"

    otherMessages.onNext("green")
    node.children(0).asInstanceOf[html.Element].style.color shouldBe "green"
  }

  test("The HTML DSL should render composite VNodes properly") {
    val items = Seq("one", "two", "three")
    val vNode = div(items.map(item => span(item)))

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<div><span>one</span><span>two</span><span>three</span></div>"

  }

  test("The HTML DSL should render nodes with only attribute receivers properly") {
    val classes = PublishSubject[String]
    val vNode = button( className <-- classes, "Submit")

    val node = document.createElement("div")
    document.body.appendChild(node)
    OutWatch.renderInto(node, vNode).unsafeRunSync()

    classes.onNext("active")

    node.innerHTML shouldBe """<button class="active">Submit</button>"""
  }


  test("The HTML DSL should work with custom tags") {

    val vNode = div(tag("main")())

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderReplace(node, vNode).unsafeRunSync()

    node.innerHTML shouldBe "<main></main>"
  }


  test("The HTML DSL should work with un-assigned booleans attributes and props") {

    val vNode = option(selected, disabled)

    val node = document.createElement("option").asInstanceOf[html.Option]
    document.body.appendChild(node)

    node.selected shouldBe false
    node.disabled shouldBe false

    OutWatch.renderReplace(node, vNode).unsafeRunSync()

    node.selected shouldBe true
    node.disabled shouldBe true
  }

  test("The HTML DSL should correctly work with AsVomModifier conversions") {

    val node = document.createElement("div")

    OutWatch.renderReplace(node, div("one")).unsafeRunSync()
    node.innerHTML shouldBe "one"

    OutWatch.renderReplace(node, div(Some("one"))).unsafeRunSync()
    node.innerHTML shouldBe "one"

    val node2 = document.createElement("div")
    OutWatch.renderReplace(node2, div(Option.empty[Int])).unsafeRunSync()
    node2.innerHTML shouldBe ""

    OutWatch.renderReplace(node, div(1)).unsafeRunSync()
    node.innerHTML shouldBe "1"

    OutWatch.renderReplace(node, div(1.0)).unsafeRunSync()
    node.innerHTML shouldBe "1"

    OutWatch.renderReplace(node, div(Seq("one", "two"))).unsafeRunSync()
    node.innerHTML shouldBe "onetwo"

    OutWatch.renderReplace(node, div(Seq(1, 2))).unsafeRunSync()
    node.innerHTML shouldBe "12"

    OutWatch.renderReplace(node, div(Seq(1.0, 2.0))).unsafeRunSync()
    node.innerHTML shouldBe "12"
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

  test("Child stream should work for string options") {
    val myOption: Handler[Option[String]] = Handler.create(Option("a")).unsafeRunSync()
    val node = div(id := "strings",
      myOption
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "a"

    myOption.unsafeOnNext(None)
    element.innerHTML shouldBe ""
  }

  test("Child stream should work for vnode options") {
    val myOption: Handler[Option[VNode]] = Handler.create(Option(div("a"))).unsafeRunSync()
    val node = div(id := "strings",
      myOption
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>a</div>"

    myOption.unsafeOnNext(None)
    element.innerHTML shouldBe ""
  }

  test("LocalStorage should provide a handler") {

    val key = "banana"
    val triggeredHandlerEvents = mutable.ArrayBuffer.empty[Option[String]]

    localStorage.getItem(key) shouldBe null

    val storageHandler = util.LocalStorage.handler(key).unsafeRunSync()
    storageHandler.foreach { e => triggeredHandlerEvents += e }
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None)

    storageHandler.unsafeOnNext(Some("joe"))
    localStorage.getItem(key) shouldBe "joe"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"))

    var initialValue: Option[String] = null
    util.LocalStorage.handler(key).unsafeRunSync().foreach {
      initialValue = _
    }
    initialValue shouldBe Some("joe")

    storageHandler.unsafeOnNext(None)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None)

    // localStorage.setItem(key, "split") from another window
    dispatchStorageEvent(key, newValue = "split", null)
    localStorage.getItem(key) shouldBe "split"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"))

    // localStorage.removeItem(key) from another window
    dispatchStorageEvent(key, null, "split")
    localStorage.getItem(key) shouldBe "null"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None)

    // only trigger handler if value changed
    storageHandler.unsafeOnNext(None)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None)

    storageHandler.unsafeOnNext(Some("rhabarbar"))
    localStorage.getItem(key) shouldBe "rhabarbar"
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar"))

    // localStorage.clear() from another window
    dispatchStorageEvent(null, null, null)
    localStorage.getItem(key) shouldBe null
    triggeredHandlerEvents.toList shouldBe List(None, Some("joe"), None, Some("split"), None, Some("rhabarbar"), None)
  }

  test("Modifier stream should work for modifier") {
    val myHandler = Handler.create[VDomModifier](Seq(cls := "hans", b("stark"))).unsafeRunSync()
    val node = div(id := "strings",
      div(myHandler)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div class="hans"><b>stark</b></div>"""

    myHandler.unsafeOnNext(Option(id := "fair"))
    element.innerHTML shouldBe """<div id="fair"></div>"""
  }

  test("Modifier stream should work for multiple mods") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(myHandler, "bla")
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div>bla</div>"

    myHandler.unsafeOnNext(cls := "hans")
    element.innerHTML shouldBe """<div class="hans">bla</div>"""

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(div(
      innerHandler,
      cls := "no?",
      "yes?"
    ))

    element.innerHTML shouldBe """<div><div class="no?">yes?</div>bla</div>"""

    innerHandler.unsafeOnNext(Seq(span("question:"), id := "heidi"))
    element.innerHTML shouldBe """<div><div class="no?" id="heidi"><span>question:</span>yes?</div>bla</div>"""

    myHandler.unsafeOnNext(div(
      innerHandler,
      cls := "no?",
      "yes?",
      b("go!")
    ))

    element.innerHTML shouldBe """<div><div class="no?">yes?<b>go!</b></div>bla</div>"""

    innerHandler.unsafeOnNext(Seq(span("question and answer:"), id := "heidi"))
    element.innerHTML shouldBe """<div><div class="no?" id="heidi"><span>question and answer:</span>yes?<b>go!</b></div>bla</div>"""

    myHandler.unsafeOnNext(Seq(span("nope")))
    element.innerHTML shouldBe """<div><span>nope</span>bla</div>"""

    innerHandler.unsafeOnNext(b("me?"))
    element.innerHTML shouldBe """<div><span>nope</span>bla</div>"""
  }

  test("Modifier stream should work for nested modifier stream receiver") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(myHandler)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    val innerHandler = Handler.create[VDomModifier]().unsafeRunSync()
    myHandler.unsafeOnNext(innerHandler)
    element.innerHTML shouldBe """<div></div>"""

    innerHandler.unsafeOnNext(VDomModifier(cls := "hans", "1"))
    element.innerHTML shouldBe """<div class="hans">1</div>"""

    val innerHandler2 = Handler.create[VDomModifier](IO.pure(EmptyModifier)).unsafeRunSync()
    myHandler.unsafeOnNext(IO.pure(ModifierStream(innerHandler2)))
    element.innerHTML shouldBe """<div></div>"""

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(ModifierStream(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div></div>"""

    myHandler.unsafeOnNext(IO.pure(CompositeModifier(StringVNode("pete") :: ModifierStream(innerHandler2) :: Nil)))
    element.innerHTML shouldBe """<div>pete</div>"""
    innerHandler2.unsafeOnNext(VDomModifier(id := "dieter", "r"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""

    innerHandler.unsafeOnNext(b("me?"))
    element.innerHTML shouldBe """<div id="dieter">peter</div>"""

    myHandler.unsafeOnNext(span("the end"))
    element.innerHTML shouldBe """<div><span>the end</span></div>"""
  }


  test("Modifier stream should work for nested observables with seq modifiers ") {
    val innerHandler = Handler.create("b").unsafeRunSync()
    val outerHandler = Handler.create(Seq[VDomModifier]("a", data.test := "v", innerHandler)).unsafeRunSync
    val node = div(
      id := "strings",
      outerHandler
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.outerHTML shouldBe """<div id="strings" data-test="v">ab</div>"""

    innerHandler.unsafeOnNext("c")
    element.outerHTML shouldBe """<div id="strings" data-test="v">ac</div>"""

    outerHandler.unsafeOnNext(Seq[VDomModifier]("meh"))
    element.outerHTML shouldBe """<div id="strings">meh</div>"""
  }

  test("Modifier stream should work for nested observables with seq modifiers and attribute stream") {
    val innerHandler = Handler.create[String]().unsafeRunSync()
    val outerHandler = Handler.create(Seq[VDomModifier]("a", data.test := "v", href <-- innerHandler)).unsafeRunSync()
    val node = div(
      id := "strings",
      outerHandler
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.outerHTML shouldBe """<div id="strings" data-test="v">a</div>"""

    innerHandler.unsafeOnNext("c")
    element.outerHTML shouldBe """<div id="strings" data-test="v" href="c">a</div>"""

    innerHandler.unsafeOnNext("d")
    element.outerHTML shouldBe """<div id="strings" data-test="v" href="d">a</div>"""

    outerHandler.unsafeOnNext(Seq[VDomModifier]("meh"))
    element.outerHTML shouldBe """<div id="strings">meh</div>"""
  }

  test("Modifier stream should work for double nested modifier stream receiver") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(myHandler)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.unsafeOnNext(IO.pure(ModifierStream(Observable[VDomModifier](IO.pure(ModifierStream(Observable[VDomModifier](cls := "hans")))))))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  test("Modifier stream should work for triple nested modifier stream receiver") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStream(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.unsafeOnNext(IO.pure(ModifierStream(Observable[VDomModifier](IO.pure(ModifierStream(Observable[VDomModifier](IO.pure(ModifierStream(Observable(cls := "hans"))))))))))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  test("Modifier stream should work for multiple nested modifier stream receiver") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(IO.pure(ModifierStream(myHandler)))
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.unsafeOnNext(IO.pure(ModifierStream(
      Observable[VDomModifier](
        VDomModifier(
          IO.pure(ModifierStream(Observable[VDomModifier]("a"))),
          IO.pure(ModifierStream(Observable(span("b")))))
      ))))
    element.innerHTML shouldBe """<div>a<span>b</span></div>"""
  }


  test("Modifier stream should work for nested attribute stream receiver") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(myHandler)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe "<div></div>"

    myHandler.unsafeOnNext(cls <-- Observable("hans"))
    element.innerHTML shouldBe """<div class="hans"></div>"""
  }

  test("Modifier stream should work for nested emitter") {
    val myHandler = Handler.create[VDomModifier]().unsafeRunSync()
    val node = div(id := "strings",
      div(id := "click", myHandler)
    )

    OutWatch.renderInto("#app", node).unsafeRunSync()

    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div id="click"></div>"""

    var clickCounter = 0
    myHandler.unsafeOnNext(onClick --> sideEffect(_ => clickCounter += 1))
    element.innerHTML shouldBe """<div id="click"></div>"""

    clickCounter shouldBe 0
    val event = document.createEvent("Events")
    initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
    document.getElementById("click").dispatchEvent(event)
    clickCounter shouldBe 1
  }

  test("Modifier stream should work for streaming accum attributes") {
    val myClasses = Handler.create[String]("second").unsafeRunSync()
    val myClasses2 = Handler.create[String]().unsafeRunSync()
    val node = div(
      id := "strings",
      div(
        cls := "first",
        myClasses.map { cls := _ },
        modifiers(
          cls <-- myClasses2
        )
      )
    )
    OutWatch.renderInto("#app", node).unsafeRunSync()
    val element = document.getElementById("strings")
    element.innerHTML shouldBe """<div class="first second"></div>"""
    myClasses2.unsafeOnNext("third")
    element.innerHTML shouldBe """<div class="first second third"></div>"""
    myClasses2.unsafeOnNext("more")
    element.innerHTML shouldBe """<div class="first second more"></div>"""
    myClasses.unsafeOnNext("yeah")
    element.innerHTML shouldBe """<div class="first yeah more"></div>"""
  }


  test("Modifier stream should work correctly with nested streams and patching") {

    val handler = Handler.create[Int](0).unsafeRunSync
    val handler2 = Handler.create[Int](0).unsafeRunSync
    val handler3 = Handler.create[Int](0).unsafeRunSync

    val vtree = div( id := "main",
      handler.map { i =>
        (0 to i).map { _ =>
          div()
        }
      },
      handler2.map { i =>
        (0 to i).map { _ =>
          div(
            div(handler3)
          )
        }
      }
    )

    val node = document.createElement("div")
    document.body.appendChild(node)

    OutWatch.renderInto(node, vtree).unsafeRunSync()

    handler.unsafeOnNext(1)
    handler2.unsafeOnNext(1)
    handler3.unsafeOnNext(1)

    node.innerHTML shouldBe "<div id=\"main\"><div></div><div></div><div><div>1</div></div><div><div>1</div></div></div>"
  }

}

package outwatch

import outwatch.dom._
import outwatch.dom.dsl._

import scala.scalajs.js

object AttributeSpec extends JSDomSuite {

  testEffect("class attributes should be merged") {
    val node = input(
      className := "class1",
      cls := "class2"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map("class" -> "class1 class2")
    }
  }

  testEffect("class attributes should be merged when given as sequences") {
    val node = input(
      cls := Seq("class1", "class2")
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map("class" -> "class1 class2")
    }
  }

  testEffect("custom attributes should be able to be accumulated") {

    val node = input(
      attr("id").accum(",") := "foo1",
      attr("id").accum(",") := "foo2"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toList shouldBe List("id" -> "foo1,foo2")
    }
  }



  testEffect("data attributes should be able to be accumulated") {

    val node = input(
      data.foo.accum(",") := "foo1",
      data.foo.accum(",") := "foo2"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toList shouldBe List("data-foo" -> "foo1,foo2")
    }
  }

  testEffect("data attribute should correctly render only data") {
    val node = input(
      data.geul := "bar",
      data.geuli.gurk := "barz"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map("data-geul" -> "bar", "data-geuli-gurk" -> "barz")
    }
  }

  testEffect("data attribute should correctly render expanded data with dynamic content") {
    val node = input(
      dataAttr("geul") := "bar",
      dataAttr("geuli-gurk") := "barz"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map("data-geul" -> "bar", "data-geuli-gurk" -> "barz")
    }
  }

//  it should "not compile data.without suffix" in {
//    """input(data.:= "bar")""" shouldNot compile
//  }


  testEffect("attr/prop/style should correctly render type") {
    val node = tag("input")(
      attr("foo") := "foo",
      attr[Boolean]("boo", identity) := true,
      attr[Boolean]("yoo", x => if (x) "yes" else "no") := true,
      prop("bar") := "bar",
      prop("num") := 12,
      style("baz") := "baz",
      contentEditable := false,
      unselectable := false,
      disabled := false
    ).map(_.toSnabbdom)

    node.map { node =>

      node.data.attrs.toList shouldBe List(
        "foo" -> "foo",
        "boo" -> true,
        "yoo" -> "yes",
        "contenteditable" -> "false",
        "unselectable" -> "off",
        "disabled" -> false
      )

      node.data.props.toList shouldBe List("bar" -> "bar", "num" -> 12)

      node.data.style.toList shouldBe List("baz" -> "baz")
    }
  }

  testEffect("optional attributes should  correctly render") {
    val node = input(
      data.foo := Option("bar"),
      data.bar := Option.empty[String]
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map("data-foo" -> "bar")
    }
  }

  testEffect("apply on vtree should correctly merge attributes") {
    val node = input(
      data.a := "bar",
      data.a.gurke := "franz"
    )(
      data.a := "buh",
      data.a.tomate := "gisela"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.attrs.toMap shouldBe Map(
        "data-a" -> "buh",
        "data-a-gurke" -> "franz",
        "data-a-tomate" -> "gisela"
      )
    }
  }

  testEffect("apply on vtree should correctly merge styles written with style") {
    val node = input(
      style("color") := "red",
      fontSize := Seq("1px", "5px")  // 5px will override 1px
    )(
      style("color") := "blue",
      border := "1px solid black"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.style.toMap shouldBe Map(
        ("color", "blue"),
        ("font-size", "5px"),
        ("border", "1px solid black")
      )
    }

  }

  testEffect("apply on vtree should correctly merge styles") {
    val node = input(
      color.red,
      fontSize:= "5px"
    )(
      color.blue,
      border := "1px solid black"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.style.toMap shouldBe Map(
        ("color", "blue"),
        ("font-size", "5px"),
        ("border", "1px solid black")
      )
    }
  }

  testEffect("apply on vtree should correctly merge keys") {
    val node = input( key := "bumm")( key := "klapp").map(_.toSnabbdom)
    val node2 = input()( key := "klapp").map(_.toSnabbdom)
    val node3 = input( key := "bumm")().map(_.toSnabbdom)

    for {
      node <- node
      node2 <- node2
      node3 <- node3
    } yield {
      node.data.key.toList shouldBe List("klapp")
      node2.data.key.toList shouldBe List("klapp")
      node3.data.key.toList shouldBe List("bumm")
    }
  }

  testEffect("style attribute should render correctly") {
    val node = input(color.red).map(_.toSnabbdom)

    node.map { node =>
      node.data.style.toMap shouldBe Map(
        "color" -> "red"
      )
    }
  }


  testEffect("extended styles should convert correctly") {
    val node = div(
      opacity := 0,
      opacity.delayed := 1,
      opacity.remove := 0,
      opacity.destroy := 0
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.style("opacity") shouldBe "0"
      node.data.style("delayed").asInstanceOf[js.Dictionary[String]].toMap shouldBe Map("opacity" -> "1")
      node.data.style("remove").asInstanceOf[js.Dictionary[String]].toMap shouldBe Map("opacity" -> "0")
      node.data.style("destroy").asInstanceOf[js.Dictionary[String]].toMap shouldBe Map("opacity" -> "0")
    }
  }

  testEffect("style accum should convert correctly") {
    val node = div(
      transition := "transform .2s ease-in-out",
      transition.accum(",") := "opacity .2s ease-in-out"
    ).map(_.toSnabbdom)

    node.map { node =>
      node.data.style.toMap shouldBe Map(
        "transition" -> "transform .2s ease-in-out,opacity .2s ease-in-out"
      )
    }
  }

  testEffect("svg should should work with tags and attributes") {
    import outwatch.dom.dsl.svg._
    val node = svg(
      path(fill := "red", d := "M 100 100 L 300 100 L 200 300 z")
    ).map(_.toSnabbdom)

    node.map { node =>
      node.children.get.head.data.attrs.toMap shouldBe Map("fill" -> "red", "d" -> "M 100 100 L 300 100 L 200 300 z")
    }
  }
}

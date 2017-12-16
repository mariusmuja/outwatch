package outwatch

import outwatch.dom._
import outwatch.dom.dsl._

import scala.scalajs.js

object AttributeSpec extends JSDomSuite {


  test("class attributes should be merged") {
    val node = input(
      className := "class1",
      cls := "class2"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(node.data.attrs.toMap, Map("class" -> "class1 class2"))
  }

  test("custom attributes should be able to be accumulated") {

    val node = input(
      attr("id").accum(",") := "foo1",
      attr("id").accum(",") := "foo2"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(node.data.attrs.toList, List("id" -> "foo1,foo2"))
  }

  test("data attributes should be able to be accumulated") {

    val node = input(
      data.foo.accum(",") := "foo1",
      data.foo.accum(",") := "foo2"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(node.data.attrs.toList, List("data-foo" -> "foo1,foo2"))
  }

  test("data attribute should correctly render only data") {
    val node = input(
      data.geul := "bar",
      data.geuli.gurk := "barz"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap,
      Map(
        "data-geul" -> "bar",
        "data-geuli-gurk" -> "barz"
      )
    )
  }

  test("data attribute should correctly render expanded data with dynamic content") {
    val node = input(
      dataAttr("geul") := "bar",
      dataAttr("geuli-gurk") := "barz"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap,
      Map(
        "data-geul" -> "bar",
        "data-geuli-gurk" -> "barz"
      )
    )
  }

//  it should "not compile data.without suffix" in {
//    """input(data.:= "bar")""" shouldNot compile
//  }


  test("attr/prop/style should correctly render type") {
    val node = tag("input")(
      attr("foo") := "foo",
      attr[Boolean]("boo", identity) := true,
      attr[Boolean]("yoo", x => if (x) "yes" else "no") := true,
      prop("bar") := "bar",
      prop("num") := 12,
      style("baz") := "baz",
      contentEditable := false,
      autoComplete := false,
      disabled := false
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.attrs.toList,
      List(
        "foo" -> "foo",
        "boo" -> true,
        "yoo" -> "yes",
        "contenteditable" -> "false",
        "autocomplete" -> "off",
        "disabled" -> false
      )
    )
    assertEquals(
      node.data.props.toList,
      List(
        "bar" -> "bar",
        "num" -> 12
      )
    )
    assertEquals(
      node.data.style.toList,
      List(
        "baz" -> "baz"
      )
    )
  }

  test("optional attributes should correctly render") {
    val node = input(
      data.foo :=? Option("bar"),
      data.bar :=? Option.empty[String]
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap, Map("data-foo" -> "bar")
    )
  }

  test("apply on vtree should correctly merge attributes") {
    val node = input(
      data.a := "bar",
      data.a.gurke := "franz"
    )(
      data.a := "buh",
      data.a.tomate := "gisela"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap,
      Map(
        "data-a" -> "buh",
        "data-a-gurke" -> "franz",
        "data-a-tomate" -> "gisela"
      )
    )
  }

  test("apply on vtree should correctly merge styles written with style") {
    val node = input(
      style("color") := "red",
      fontSize := "5px"
    )(
      style("color") := "blue",
      border := "1px solid black"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        ("color", "blue"),
        ("font-size", "5px"),
        ("border", "1px solid black")
      )
    )
  }

  test("apply on vtree should correctly merge styles") {
    val node = input(
      color.red,
      fontSize:= "5px"
    )(
      color.blue,
      border := "1px solid black"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        ("color", "blue"),
        ("font-size", "5px"),
        ("border", "1px solid black")
      )
    )
  }

  test("apply on vtree should correctly merge keys") {
    val node = input( key := "bumm")( key := "klapp").map(_.toSnabbdom).unsafeRunSync()
    assertEquals(node.data.key.toList, List("klapp"))

    val node2 = input()( key := "klapp").map(_.toSnabbdom).unsafeRunSync()
    assertEquals(node2.data.key.toList, List("klapp"))

    val node3 = input( key := "bumm")().map(_.toSnabbdom).unsafeRunSync()
    assertEquals(node3.data.key.toList, List("bumm"))
  }

  test("style attribute should render correctly") {
    val node = input(color.red).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        "color" -> "red"
      )
    )
  }


  test("extended styles should convert correctly") {
    val node = div(
      opacity := 0,
      opacity.delayed := 1,
      opacity.remove := 0,
      opacity.destroy := 0
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(node.data.style("opacity"), "0")
    assertEquals(node.data.style("delayed").asInstanceOf[js.Dictionary[String]].toMap, Map("opacity" -> "1"))
    assertEquals(node.data.style("remove").asInstanceOf[js.Dictionary[String]].toMap, Map("opacity" -> "0"))
    assertEquals(node.data.style("destroy").asInstanceOf[js.Dictionary[String]].toMap, Map("opacity" -> "0"))
  }

  test("style accum should convert correctly") {
    val node = div(
      transition := "transform .2s ease-in-out",
      transition.accum(",") := "opacity .2s ease-in-out"
    ).map(_.toSnabbdom).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        "transition" -> "transform .2s ease-in-out,opacity .2s ease-in-out"
      )
    )
  }
}

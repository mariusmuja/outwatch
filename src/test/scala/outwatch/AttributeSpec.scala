package outwatch

import minitest._
import outwatch.dom._

object AttributeSpec extends SimpleTestSuite {


  test("class attributes should be merged") {
    val node = input(
      cls.spaceAccum := "class1",
      cls.spaceAccum := "class2"
    ).map(_.asProxy).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap,
      Map(
        "class" -> "class1 class2"
      )
    )
  }


  test("data attribute should correctly render only data") {
    val node = input(
      data.geul := "bar",
      data.geuli.gurk := "barz"
    ).map(_.asProxy).unsafeRunSync()

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
    ).map(_.asProxy).unsafeRunSync()

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
    val node = input(
      attr("foo") := "foo",
      attr[Boolean]("boo", identity) := true,
      attr[Boolean]("yoo", x => if (x) "yes" else "no") := true,
      prop("bar") := "bar",
      prop("num") := 12,
      style("baz") := "baz",
      contentEditable := false,
      autoComplete := false,
      disabled := false
    ).map(_.asProxy).unsafeRunSync()

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
    ).map(_.asProxy).unsafeRunSync()

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
    ).map(_.asProxy).unsafeRunSync()

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
    ).map(_.asProxy).unsafeRunSync()

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
    ).map(_.asProxy).unsafeRunSync()

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
    val node = input( dom.key := "bumm")( dom.key := "klapp").map(_.asProxy).unsafeRunSync()
    assertEquals(node.data.key.toList, List("klapp"))

    val node2 = input()( dom.key := "klapp").map(_.asProxy).unsafeRunSync()
    assertEquals(node2.data.key.toList, List("klapp"))

    val node3 = input( dom.key := "bumm")().map(_.asProxy).unsafeRunSync()
    assertEquals(node3.data.key.toList, List("bumm"))
  }

  test("style attribute should render correctly") {
    val node = input(color.red).map(_.asProxy).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        "color" -> "red"
      )
    )
  }
}

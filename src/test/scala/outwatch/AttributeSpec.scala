package outwatch

import minitest._
import outwatch.dom._

object AttributeSpec extends SimpleTestSuite {

  test("data attribute should correctly render only data") {
    val node = input(data := "bar").map(_.asProxy).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap, Map("data" -> "bar")
    )
  }

  test("data attribute should correctly render expanded data with dynamic content") {
    val node = input(data.foo := "bar").map(_.asProxy).unsafeRunSync()

    assertEquals(
      node.data.attrs.toMap, Map("data-foo" -> "bar")
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
    val node = input(data := "bar",
      data.gurke := "franz")(data := "buh", data.tomate := "gisela").map(_.asProxy).unsafeRunSync()


    assertEquals(
      node.data.attrs.toMap,
      Map(
        "data" -> "buh",
        "data-gurke" -> "franz",
        "data-tomate" -> "gisela"
      )
    )
  }

  test("apply on vtree should correctly merge styles") {
    val node = input(
      stl("color") := "red",
      stl("font-size") := "5px"
    )(
      stl("color") := "blue",
      stl("border") := "1px solid black"
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
    val node = input(stl("color") := "red").map(_.asProxy).unsafeRunSync()

    assertEquals(
      node.data.style.toMap,
      Map(
        "color" -> "red"
      )
    )
  }
}

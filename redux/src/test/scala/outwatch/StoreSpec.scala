package outwatch


import org.scalajs.dom.document
import outwatch.EventExt._
import outwatch.dom._
import outwatch.dom.dsl._
import outwatch.redux.Store

object StoreSpec extends JSDomSuite {

  test("A simple counter application that uses Store should work as intended") {

    sealed trait Action
    case object Plus extends Action
    case object Minus extends Action

    type State = Int

    def reduce(state: State, action: Action): State = action match {
      case Plus => state + 1
      case Minus => state - 1
    }

    val node = for {
      store <- Store.create[Action, State](0)(reduce _)

      div <- div(
        div(
          button(id := "plus", "+", onClick(Plus).-->(store: Sink[Action])),
          button(id := "minus", "-", onClick(Minus) --> store),
          span(id := "counter", store)
        )
      )
    } yield div

    val root = document.createElement("div")
    document.body.appendChild(root)

    OutWatch.renderInto(root, node).unsafeRunSync()

    val event = document.createEvent("Events")
    event.initEvent("click", canBubbleArg = true, cancelableArg = false)

    document.getElementById("counter").innerHTML shouldBe 0.toString

    document.getElementById("minus").dispatchEvent(event)

    document.getElementById("counter").innerHTML shouldBe (-1).toString

    for (i <- 0 to 10) {
      document.getElementById("plus").dispatchEvent(event)
      document.getElementById("counter").innerHTML shouldBe i.toString
    }
  }
}

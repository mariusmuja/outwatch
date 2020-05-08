package outwatch

import org.scalajs.dom.Event

import scala.scalajs.js

@js.native
trait EventExt extends Event {
  def initEvent(eventTypeArg: String, canBubbleArg: Boolean, cancelableArg: Boolean): Unit = js.native
}


object EventExt {
  implicit def eventExt(e: Event): EventExt = e.asInstanceOf[EventExt]
}
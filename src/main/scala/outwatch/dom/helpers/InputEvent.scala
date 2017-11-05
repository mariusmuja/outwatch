package outwatch.dom.helpers

import org.scalajs.dom.raw.{Event, HTMLInputElement}


class InputEvent() extends Event {
  override def target = {
    super.target.asInstanceOf[HTMLInputElement]
  }
}

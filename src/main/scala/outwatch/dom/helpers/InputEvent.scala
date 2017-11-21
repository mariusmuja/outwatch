package outwatch.dom.helpers

import org.scalajs.dom.Event
import org.scalajs.dom.html


class InputEvent() extends Event {
  override def target = {
    super.target.asInstanceOf[html.Input]
  }
}

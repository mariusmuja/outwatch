package outwatch.dom

import org.scalajs.dom.{ClipboardEvent, DragEvent, KeyboardEvent, MouseEvent}

/**
  * Trait containing event handlers, so they can be mixed in to other objects if needed.
  */

trait HandlerFactories extends Handlers {

  implicit class HandlerCreateHelpers(handler: Handler.type) {
    lazy val mouseEvents = Handler.create[MouseEvent]
    lazy val keyboardEvents = Handler.create[KeyboardEvent]
    lazy val dragEvents = Handler.create[DragEvent]
    lazy val clipboardEvents = Handler.create[ClipboardEvent]
  }

}


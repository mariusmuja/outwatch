package outwatch.dom

import cats.effect.IO
import org.scalajs.dom.{ClipboardEvent, DragEvent, KeyboardEvent, MouseEvent}

trait Handlers {
  @deprecated("Use Handler.create[MouseEvent] instead", "0.11.0")
  def createMouseHandler() = Handler.create[MouseEvent]
  @deprecated("Use Handler.create[KeyboardEvent] instead", "0.11.0")
  def createKeyboardHandler() = Handler.create[KeyboardEvent]
  @deprecated("Use Handler.create[DragEvent] instead", "0.11.0")
  def createDragHandler() = Handler.create[DragEvent]
  @deprecated("Use Handler.create[ClipboardEvent] instead", "0.11.0")
  def createClipboardHandler() = Handler.create[ClipboardEvent]

  @deprecated("Use Handler.create[String] instead", "0.11.0")
  def createStringHandler(defaultValues: String*) = Handler.create[String](defaultValues: _*)
  @deprecated("Use Handler.create[Boolean] instead", "0.11.0")
  def createBoolHandler(defaultValues: Boolean*) = Handler.create[Boolean](defaultValues: _*)
  @deprecated("Use Handler.create[Double] instead", "0.11.0")
  def createNumberHandler(defaultValues: Double*) = Handler.create[Double](defaultValues: _*)

  @deprecated("Use Handler.create[T] instead", "0.11.0")
  def createHandler[T](defaultValues: T*): IO[Pipe[T, T]] = Handler.create[T](defaultValues: _*)
}

trait AttributesExtra { self: Attributes =>
  lazy val `class` = className

  lazy val `for` = forId
}

trait AttributesCompat { self: Attributes =>
  @deprecated("Use `type`, tpe or typ instead", "0.11.0")
  lazy val inputType = tpe

  @deprecated("Use styleAttr instead", "0.11.0")
  lazy val style = styleAttr

  @deprecated("Use contentAttr instead", "0.11.0")
  lazy val content = contentAttr

  @deprecated("Use listId instead", "0.11.0")
  lazy val list = listId

  @deprecated("Use onClick instead", "0.11.0")
  lazy val click = onClick

  @deprecated("Use onKeyDown instead", "0.11.0")
  lazy val keydown = onKeyDown

  @deprecated("Use onInsert instead", "0.11.0")
  lazy val insert = onInsert

  @deprecated("Use onPrepatch instead", "0.11.0")
  lazy val prepatch = onPrepatch

  @deprecated("Use onUpdate instead", "0.11.0")
  lazy val update = onUpdate

  @deprecated("Use onPostpatch instead", "0.11.0")
  lazy val postpatch = onPostpatch

  @deprecated("Use onDestroy instead", "0.11.0")
  lazy val destroy = onDestroy
}

trait TagsCompat { self: Tags =>
  @deprecated("Use textArea instead", "0.11.0")
  lazy val textarea = textArea
}

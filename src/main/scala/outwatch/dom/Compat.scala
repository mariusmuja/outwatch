package outwatch.dom

import outwatch.dom.helpers.ChildStreamBuilder

trait AttributesCompat { self: Attributes =>

  @deprecated("Use `type`, tpe or typ instead", "0.11.0")
  lazy val inputType = tpe

  @deprecated("Use styleAttr instead", "0.11.0")
  lazy val style = styleAttr

  @deprecated("Use contentAttr instead", "0.11.0")
  lazy val content = contentAttr

  @deprecated("Use listId instead", "0.11.0")
  lazy val list = listId

  @deprecated("Use onInput.value instead", "0.11.0")
  lazy val inputString = onInput.value

  @deprecated("Use onInput.valueAsNumber instead", "0.11.0")
  lazy val inputNumber = onInput.valueAsNumber

  @deprecated("Use onChange.checked instead", "0.11.0")
  lazy val inputChecked = onChange.checked

  @deprecated("Use onClick instead", "0.11.0")
  lazy val click = onClick

  @deprecated("Use onKeyDown instead", "0.11.0")
  lazy val keydown = onKeyDown

  @deprecated("Use onInsert instead", "0.11.0")
  lazy val insert = onInsert

  @deprecated("Use onPrePatch instead", "0.11.0")
  lazy val prepatch = onPrePatch

  @deprecated("Use onUpdate instead", "0.11.0")
  lazy val update = onUpdate

  @deprecated("Use onPostPatch instead", "0.11.0")
  lazy val postpatch = onPostPatch

  @deprecated("Use onDestroy instead", "0.11.0")
  lazy val destroy = onDestroy
}

trait TagsCompat { self: Tags =>
  @deprecated("Use textArea instead", "0.11.0")
  lazy val textarea = textArea
}


/** OutWatch specific attributes used to asign child nodes to a VNode. */
trait OutWatchChildAttributesCompat {
  /** A special attribute that takes a stream of single child nodes. */
  lazy val child    = ChildStreamBuilder
}
package outwatch.util

import cats.effect.IO
import outwatch.dom.{Attribute, AttributeStreamReceiver, Observable, TitledAttribute, VDomModifier}


object SyntaxSugar {

  implicit class BooleanSelector(val values: Observable[Boolean]) extends AnyVal {
    def ?=(attr: IO[TitledAttribute]): IO[AttributeStreamReceiver] = {
      attr.map { attr =>
        val attributes = values.map(b => if (b) attr else Attribute.empty)
        AttributeStreamReceiver(attr.title, attributes)
      }
    }
  }


  implicit class BooleanVDomSelector(val condition: Boolean) extends AnyVal {
    def ?=[T](vDomModifier: => T)(implicit conv: T => VDomModifier): VDomModifier = {
      if (condition) conv(vDomModifier) else VDomModifier.empty
    }
  }


  implicit class OptionalVDomModifier[T](vDomModifier: => T)(implicit conv: T => VDomModifier) {
    def when(condition: Boolean): VDomModifier = {
      if (condition) conv(vDomModifier) else VDomModifier.empty
    }
  }

}

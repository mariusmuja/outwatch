package outwatch.util

import cats.effect.IO
import outwatch.dom.{Attribute, AttributeStreamReceiver, Observable, TitledAttribute, VDomModifier}


object SyntaxSugar {

  implicit class BooleanSelector(val conditionStream: Observable[Boolean]) extends AnyVal {
    def ?=(attr: IO[TitledAttribute]): IO[AttributeStreamReceiver] = {
      attr.map { attr =>
        val attributeStream = conditionStream.map(condition => if (condition) attr else Attribute.empty)
        AttributeStreamReceiver(attr.title, attributeStream)
      }
    }
  }


  implicit class BooleanVDomSelector(val condition: Boolean) extends AnyVal {
    def ?=[T](vDomModifier: => T)(implicit conv: T => VDomModifier): VDomModifier = {
      if (condition) conv(vDomModifier) else VDomModifier.empty
    }
  }


  implicit class OptionalVDomModifier[T](vDomModifier: => T) {
    def when(condition: Boolean)(implicit conv: T => VDomModifier): VDomModifier = {
      if (condition) conv(vDomModifier) else VDomModifier.empty
    }
  }

}

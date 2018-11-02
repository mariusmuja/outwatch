package outwatch.util

import outwatch.dom.{IO, ModifierStream, Observable, VDomModifier}


object SyntaxSugar {

  implicit class BooleanSelector(val conditionStream: Observable[Boolean]) extends AnyVal {
    def ?=(mod: VDomModifier): ModifierStream = {

      ModifierStream(
        conditionStream.map(condition => if (condition) mod else VDomModifier.empty)
      )
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

package outwatch.util

import cats.effect.IO
import outwatch.dom.{ModifierStream, Observable, VDomModifier}


object SyntaxSugar {

  implicit class BooleanSelector(val conditionStream: Observable[Boolean]) extends AnyVal {
    def ?=(mod: VDomModifier): IO[ModifierStream] = {
      mod.map { mod =>
        ModifierStream(
          conditionStream.map(condition => if (condition) IO.pure(mod) else VDomModifier.empty)
        )
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

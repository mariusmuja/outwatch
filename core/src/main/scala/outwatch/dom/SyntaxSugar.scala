package outwatch.dom

import outwatch.AsVDomModifier

private[outwatch] final class BooleanSelector(private val conditionStream: Observable[Boolean]) extends AnyVal {
  def ?=[T: AsVDomModifier](mod: => T): VDomModifier = {
    conditionStream.map(cond => if (cond) AsVDomModifier[T].asVDomModifier(mod) else VDomModifier.empty)
  }
}

private[outwatch] final class BooleanVDomSelector(private val condition: Boolean) extends AnyVal {
  def ?=[T: AsVDomModifier](vDomModifier: => T): VDomModifier = {
    if (condition) AsVDomModifier[T].asVDomModifier(vDomModifier) else VDomModifier.empty
  }
}

trait SyntaxSugar {
  implicit final def booleanStreamSelector(conditionStream: Observable[Boolean]): BooleanSelector =
    new BooleanSelector(conditionStream)

  implicit def booleanVDomSelector(condition: Boolean): BooleanVDomSelector =
    new BooleanVDomSelector(condition)
}


object SyntaxSugar extends SyntaxSugar
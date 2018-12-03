package outwatch.util

import outwatch.dom.{Observable, VDomModifier}

private[util] final class BooleanSelector(private val conditionStream: Observable[Boolean]) extends AnyVal {
  def ?=(mod: VDomModifier): VDomModifier = {
    conditionStream.map(cond => if (cond) mod else VDomModifier.empty)
  }
}

private[util] final class BooleanVDomSelector(private val condition: Boolean) extends AnyVal {
  def ?=[T](vDomModifier: => T)(implicit conv: T => VDomModifier): VDomModifier = {
    if (condition) conv(vDomModifier) else VDomModifier.empty
  }
}

trait SyntaxSugar {
  implicit final def booleanStreamSelector(conditionStream: Observable[Boolean]): BooleanSelector =
    new BooleanSelector(conditionStream)

  implicit def booleanVDomSelector(condition: Boolean): BooleanVDomSelector =
    new BooleanVDomSelector(condition)
}


object SyntaxSugar extends SyntaxSugar
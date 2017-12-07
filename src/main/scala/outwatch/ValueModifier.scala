package outwatch

import cats.effect.IO
import outwatch.dom.{Modifier, StringModifier}

trait ValueModifier[T] {
  def asModifier(value: T): IO[Modifier]
}

object ValueModifier {

  implicit object StringRenderer extends ValueModifier[String] {
    def asModifier(value: String): IO[Modifier] = IO.pure(StringModifier(value))
  }

  implicit object IntRenderer extends ValueModifier[Int] {
    def asModifier(value: Int): IO[Modifier] = IO.pure(StringModifier(value.toString))
  }

  implicit object DoubleRenderer extends ValueModifier[Double] {
    def asModifier(value: Double): IO[Modifier] = IO.pure(StringModifier(value.toString))
  }

}
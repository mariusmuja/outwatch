package outwatch

//import monix.eval.TaskLike
import outwatch.dom.{CompositeModifier, IO, ModifierStream, Observable, StringVNode, VDomModifier}

//import scala.language.higherKinds
import scala.scalajs.js

trait AsVDomModifier[-T] {
  def asVDomModifier(value: T): VDomModifier
}

object AsVDomModifier {

  @inline def apply[T](implicit avm: AsVDomModifier[T]): AsVDomModifier[T] = avm

  implicit object VDomModifierAsVDomModifier extends AsVDomModifier[VDomModifier] {
    def asVDomModifier(value: VDomModifier): VDomModifier = value
  }

  implicit object StringAsVDomModifier extends AsVDomModifier[String] {
    def asVDomModifier(value: String): VDomModifier = IO.pure(StringVNode(value))
  }

  implicit object IntAsVDomModifier extends AsVDomModifier[Int] {
    def asVDomModifier(value: Int): VDomModifier = IO.pure(StringVNode(value.toString))
  }

  implicit object DoubleAsVDomModifier extends AsVDomModifier[Double] {
    def asVDomModifier(value: Double): VDomModifier = IO.pure(StringVNode(value.toString))
  }
  
  implicit def seqModifier[T: AsVDomModifier]: AsVDomModifier[Seq[T]] =
    (value: Seq[T]) => IO.sequence(value.map(AsVDomModifier[T].asVDomModifier)).map(CompositeModifier)

  implicit def optionModifier[T: AsVDomModifier]: AsVDomModifier[Option[T]] =
    (value: Option[T]) => value.fold(VDomModifier.empty)(AsVDomModifier[T].asVDomModifier)

  implicit def undefOrModifier[T: AsVDomModifier]: AsVDomModifier[js.UndefOr[T]] =
    (value: js.UndefOr[T]) => value.fold(VDomModifier.empty)(AsVDomModifier[T].asVDomModifier)

  implicit def observableRender[T: AsVDomModifier]: AsVDomModifier[Observable[T]] =
    (stream: Observable[T]) => IO.pure(ModifierStream(stream.mapEval(AsVDomModifier[T].asVDomModifier)))

//  implicit def taskLikeRender[T: AsVDomModifier, F[_]: TaskLike]: AsVDomModifier[F[T]] =
//    (value: F[T]) => Observable.fromTaskLike(value)
}
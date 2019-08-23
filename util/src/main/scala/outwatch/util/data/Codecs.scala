package outwatch.util.data

import scala.util.Try

trait Encoder[-A] {
  def encode(a: A): Array[Byte]

  final def contramap[B](f: B => A): Encoder[B] = (b: B) => encode(f(b))
}

object Encoder {
  def apply[T](implicit E: Encoder[T]): Encoder[T] = E
}


trait Decoder[A] {

  def decode(a: Array[Byte]): Try[A]

  final def map[B](f: A => B): Decoder[B] = (a: Array[Byte]) => decode(a).map(f)

  final def mapTry[B](f: A => Try[B]): Decoder[B] = (a: Array[Byte]) => decode(a).flatMap(f)
}

object Decoder {
  def apply[T](implicit D: Decoder[T]): Decoder[T] = D
}
package outwatch

import outwatch.dom.{IO, Observable}

object Handler {

  private[outwatch] def apply[T](sink: Sink[T], source: Observable[T]): Pipe[T, T] = Pipe(sink, source)

  /**
    * This function also allows you to create initial values for your newly created Handler.
    * This is equivalent to calling `startWithMany` with the given values.
    *
    * @param seeds a sequence of initial values that the Handler will emit.
    * @tparam T the type parameter of the elements
    * @return the newly created Handler.
    */
  def create[T](seeds: T*): IO[Pipe[T, T]] = Pipe.create[T](seeds: _*)

  def create[T]: IO[Pipe[T, T]] = Pipe.create[T]

}


package outwatch

import outwatch.Sink.{ObservableSink, SubjectSink}
import outwatch.dom.{IO, Observable}


object Pipe {

  type >-->[-I, +O] = Pipe[I, O]
  type <--<[+O, -I] = Pipe[I, O]


  def apply[I, O](sink: Sink[I], source: Observable[O]): Pipe[I, O] =
    new ObservableSink[I, O](sink, source)

  /**
    * This function also allows you to create initial values for your newly created Pipe.
    * This is equivalent to calling `startWithMany` with the given values.
    *
    * @param seeds a sequence of initial values that the Pipe will emit.
    * @tparam T the type parameter of the elements
    * @return the newly created Pipe.
    */
  def create[T](seeds: T*): IO[Pipe[T, T]] = create[T].map { pipe =>
    if (seeds.nonEmpty) {
      pipe.transformSource(_.startWith(seeds))
    }
    else {
      pipe
    }
  }

  def create[T]: IO[Pipe[T, T]] = IO.deferAction { implicit scheduler =>
    IO.pure {
      val subjectSink = SubjectSink[T]()
      Pipe(subjectSink, subjectSink)
    }
  }

}

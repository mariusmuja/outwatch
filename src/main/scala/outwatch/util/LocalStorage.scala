package outwatch.util

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.reactive.OverflowStrategy.Unbounded
import outwatch.Sink
import org.scalajs.dom.window.localStorage


object LocalStorageReader {
  def apply(key: String): Observable[String] = {
    Observable.create[String](Unbounded){ observer =>
      observer.onNext(localStorage.getItem(key))
      Cancelable.empty
    }
  }
}

object LocalStorageWriter {
  def apply(key: String): Sink[String] = {
    Sink.create[String](
      data => IO {
        localStorage.setItem(key, data)
        Continue
      }
    )
  }
}

package outwatch.util

import cats.effect.IO
import monix.execution.Ack.Continue
import monix.execution.Scheduler
import org.scalajs.dom.Storage
import org.scalajs.dom.window.localStorage
import outwatch.dom.Observable
import outwatch.dom.dsl.events.window.onStorage
import outwatch.{Handler, Sink}


class StorageHandler(storageArea: Storage)(key: String)(implicit s: Scheduler) {

  def value: Observable[String] = Observable(storageArea.getItem(key))

  val handler: IO[Handler[Option[String]]] = Handler.create[Option[String]].map { handler =>

    val otherPageChange = onStorage.filter(x => x.storageArea == storageArea && x.key == key)
      .map(e => Option(e.newValue).map(_.asInstanceOf[String]))

    handler.transformSource { orig =>
      val obs = orig.map { value =>
        value.fold(storageArea.removeItem(key))(value =>
          storageArea.setItem(key, value)
        )
        value
      }.startWith(Seq(Option(storageArea.getItem(key))))

      Observable.merge(obs, otherPageChange).share
    }
  }
}

object LocalStorageReader {
  def apply(key: String): Observable[String] = Observable(localStorage.getItem(key))
}

object LocalStorageWriter {
  def apply(key: String)(implicit s: Scheduler): Sink[String] = {
    Sink.create[String](
      data => IO {
        localStorage.setItem(key, data)
        Continue
      }
    )
  }
}

package outwatch.util

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom.{CloseEvent, Event, MessageEvent}
import outwatch.Sink
import outwatch.dom.{IO, Observable}

object WebSocket {
  implicit def toSink(socket: WebSocket): IO[Sink[String]] = socket.sink
  implicit def toSource(socket: WebSocket): Observable[MessageEvent] = socket.source
}

final case class WebSocket private(url: String)(implicit s: Scheduler) {
  val ws = new org.scalajs.dom.WebSocket(url)

  lazy val source = Observable.create[MessageEvent](Unbounded)(observer => {
    ws.onmessage = (e: MessageEvent) => observer.onNext(e)
    ws.onerror = (e: Event) => observer.onError(new Exception(e.toString))
    ws.onclose = (e: CloseEvent) => observer.onComplete()
    Cancelable(() => ws.close())
  })

  lazy val sink = Sink.create[String](
    s => {
      ws.send(s)
      Continue
    },
    _ => (),
    () => ws.close()
  )

}


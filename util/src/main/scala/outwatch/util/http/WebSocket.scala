package outwatch.util.http

import java.nio.ByteBuffer

import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.reactive.OverflowStrategy.Unbounded
import org.scalajs.dom
import org.scalajs.dom.{CloseEvent, Event, MessageEvent}
import outwatch.Pipe.>-->
import outwatch.dom.{IO, Observable}
import outwatch.util.data.{Decoder, Encoder}
import outwatch.{Pipe, Sink}

import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.util.{Failure, Try}

object WebSocket {


  def apply[I: Encoder, O: Decoder](url: String): IO[I >--> Try[O]] = {
    for {
      ws <- IO {
        val ws = new org.scalajs.dom.WebSocket(url)
        ws.binaryType = "arraybuffer"
        dom.console.log("Creating websocket")
        ws
      }
      sink <- Sink.create[I](
        s => {
          val a = Encoder[I].encode(s)
          val bb = ByteBuffer.allocateDirect(a.length)
          bb.put(a)
          ws.send(bb.arrayBuffer())
          Continue
        },
        _ => (),
        () => {
          dom.console.log("Closing socket")
          ws.close()
        }
      )
    } yield {
      lazy val source = Observable.create[Try[O]](Unbounded)(observer => {
        ws.onmessage = (e: MessageEvent) => {
          val data = e.data.asInstanceOf[ArrayBuffer]
          val bb = TypedArrayBuffer.wrap(data)
          val buf = Array.ofDim[Byte](bb.remaining())
          bb.get(buf)
          observer.onNext(Decoder[O].decode(buf))
        }
        ws.onerror = (e: Event) => {
          dom.console.error("Websocket error", e)
          observer.onNext(Failure(new Exception("Websocket error")))
          observer.onError(new Exception(e.toString))
        }
        ws.onclose = (e: CloseEvent) => {
          dom.console.error("Websocket close", e)
          observer.onNext(Failure(new Exception("Connection closed")))
          observer.onComplete()
        }
        Cancelable(() => ws.close())
      })
      Pipe(sink, source)
    }
  }
}

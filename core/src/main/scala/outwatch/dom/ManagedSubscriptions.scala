package outwatch.dom

import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.execution.cancelables.CompositeCancelable
import org.scalajs.dom
import outwatch.dom.dsl.attributes.lifecycle

trait ManagedSubscriptions {

  def managed(subscription: IO[Cancelable]): VDomModifier = {
    subscription.flatMap { sub: Cancelable =>
      Sink.create[dom.Element] { _ =>
        sub.cancel()
        Continue
      }.flatMap( sink => lifecycle.onDestroy --> sink)
    }
  }

  def managed(sub1: IO[Cancelable], sub2: IO[Cancelable], subscriptions: IO[Cancelable]*): VDomModifier = {

    IO.sequence(sub1 :: sub2 :: subscriptions.toList).flatMap { subs =>
      val composite = CompositeCancelable(subs: _*)
      Sink.create[dom.Element]{ _ =>
        composite.cancel()
        Continue
      }.flatMap(sink => lifecycle.onDestroy --> sink)
    }
  }

}

object ManagedSubscriptions extends ManagedSubscriptions

package outwatch

import minitest._
import monix.execution.Ack.Continue
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

object HandlerSpec extends SimpleTestSuite {
  
  implicit class Subscriber[T](obs: Observable[T]) {
    def apply(next: T => Unit): Cancelable = obs.subscribe { t =>
      next(t)
      Continue
    }
  }
  
  test("Handler should lens") {
    val handler = Handler.create[(String, Int)].unsafeRunSync()
    val lensed = handler.lens[Int](("harals", 0))(_._2)((tuple, num) => (tuple._1, num))

    var handlerValue: (String, Int) = null
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 15)
    assertEquals(handlerValue, (("harals", 15)))

    handler.observer.onNext(("peter", 12))
    assertEquals(lensedValue, 12)
    assertEquals(handlerValue, (("peter", 12)))

    lensed.observer.onNext(-1)
    assertEquals(lensedValue, -1)
    assertEquals(handlerValue, (("peter", -1)))
  }

  test("Handler should mapSource") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.mapSource(_ - 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 14)
    assertEquals(handlerValue, 15)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 11)
    assertEquals(handlerValue, 12)
  }

  test("Handler should transformSource") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformSource(_.map(_ - 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 14)
    assertEquals(handlerValue, 15)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 11)
    assertEquals(handlerValue, 12)
  }

  test("Handler should mapSink") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.mapSink[Int](_ + 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 16)
    assertEquals(handlerValue, 16)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 12)
    assertEquals(handlerValue, 12)
  }

  test("Handler should transformSink") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformSink[Int](_.map(_ + 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 16)
    assertEquals(handlerValue, 16)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 12)
    assertEquals(handlerValue, 12)
  }

  test("Handler should imap") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.imap[Int](_ - 1)(_ + 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 15)
    assertEquals(handlerValue, 16)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 11)
    assertEquals(handlerValue, 12)
  }

  test("Handler should transformHandler") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformHandler[Int](_.map(_ - 1))(_.map(_ + 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    assertEquals(lensedValue, 15)
    assertEquals(handlerValue, 16)

    handler.observer.onNext(12)
    assertEquals(lensedValue, 11)
    assertEquals(handlerValue, 12)
  }
}

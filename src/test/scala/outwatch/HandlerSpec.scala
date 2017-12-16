package outwatch

object HandlerSpec extends JSDomSuite {
  
  test("Handler should lens") {
    val handler = Handler.create[(String, Int)].unsafeRunSync()
    val lensed = handler.lens[Int](("harals", 0))(_._2)((tuple, num) => (tuple._1, num))

    var handlerValue: (String, Int) = null
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 15
    handlerValue shouldBe (("harals", 15))

    handler.observer.onNext(("peter", 12))
    lensedValue shouldBe 12
    handlerValue shouldBe (("peter", 12))

    lensed.observer.onNext(-1)
    lensedValue shouldBe -1
    handlerValue shouldBe (("peter", -1))
  }

  test("Handler should mapSource") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.mapSource(_ - 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 14
    handlerValue shouldBe 15

    handler.observer.onNext(12)
    lensedValue shouldBe 11
    handlerValue shouldBe 12
  }

  test("Handler should transformSource") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformSource(_.map(_ - 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 14
    handlerValue shouldBe 15

    handler.observer.onNext(12)
    lensedValue shouldBe 11
    handlerValue shouldBe 12
  }

  test("Handler should mapSink") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.mapSink[Int](_ + 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 16
    handlerValue shouldBe 16

    handler.observer.onNext(12)
    lensedValue shouldBe 12
    handlerValue shouldBe 12
  }

  test("Handler should transformSink") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformSink[Int](_.map(_ + 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 16
    handlerValue shouldBe 16

    handler.observer.onNext(12)
    lensedValue shouldBe 12
    handlerValue shouldBe 12
  }

  test("Handler should imap") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.imap[Int](_ - 1)(_ + 1)

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 15
    handlerValue shouldBe 16

    handler.observer.onNext(12)
    lensedValue shouldBe 11
    handlerValue shouldBe 12
  }

  test("Handler should transformHandler") {
    val handler = Handler.create[Int].unsafeRunSync()
    val lensed = handler.transformHandler[Int](_.map(_ - 1))(_.map(_ + 1))

    var handlerValue: Int = -100
    var lensedValue: Int = -100

    handler(handlerValue = _)
    lensed(lensedValue = _)

    lensed.observer.onNext(15)
    lensedValue shouldBe 15
    handlerValue shouldBe 16

    handler.observer.onNext(12)
    lensedValue shouldBe 11
    handlerValue shouldBe 12
  }
}

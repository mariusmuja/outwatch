import outwatch.dom.Observable

package object outwatch {
  type Pipe[-I, +O] = Observable[O] with Sink[I]
  type Handler[T] = Pipe[T, T]

  implicit def toPipeOps[I, O](p: Pipe[I, O]): PipeOps[I, O] = new PipeOps(p)
  
  implicit class HandlerOps[T](private val self: Handler[T]) extends AnyVal {

    def imap[S](read: T => S)(write: S => T): Handler[S] = self.mapPipe(write)(read)

    def lens[S](seed: T)(read: T => S)(write: (T, S) => T): Handler[S] = {
      val redirected = self.redirect[S](_.withLatestFrom(self.startWith(Seq(seed))){ case (a, b) => write(b, a) })
      Handler(redirected, self.map(read))
    }

    def transformHandler[S](read: Observable[T] => Observable[S])(write: Observable[S] => Observable[T]): Handler[S] =
      self.transformPipe(write)(read)
  }
}

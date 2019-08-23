package outwatch

import outwatch.dom.Observable

class PipeOps[I, O](private val self: Pipe[I, O]) extends AnyVal {

  def mapSink[I2](f: I2 => I): Pipe[I2, O] = Pipe(self.redirectMap(f), self)

  def mapSource[O2](f: O => O2): Pipe[I, O2] = Pipe(self, self.map(f))

  def mapPipe[I2, O2](f: I2 => I)(g: O => O2): Pipe[I2, O2] = Pipe(self.redirectMap(f), self.map(g))


  def collectSink[I2](f: PartialFunction[I2, I]): Pipe[I2, O] = Pipe(self.redirect(_.collect(f)), self)

  def collectSource[O2](f: PartialFunction[O, O2]): Pipe[I, O2] = Pipe(self, self.collect(f))

  def collectPipe[I2, O2](f: PartialFunction[I2, I])(g: PartialFunction[O, O2]): Pipe[I2, O2] = Pipe(
    self.redirect(_.collect(f)), self.collect(g)
  )

  def filterSink(f: I => Boolean): Pipe[I, O] = Pipe(self.redirect(_.filter(f)), self)

  def filterSource(f: O => Boolean): Pipe[I, O] = Pipe(self, self.filter(f))


  def transformSink[I2](f: Observable[I2] => Observable[I]): Pipe[I2, O] = Pipe(self.redirect(f), self)

  def transformSource[O2](f: Observable[O] => Observable[O2]): Pipe[I, O2] = Pipe(self, f(self))

  def transformPipe[I2, O2](f: Observable[I2] => Observable[I])(g: Observable[O] => Observable[O2]): Pipe[I2, O2] =
    Pipe(self.redirect(f), g(self))
}
package outwatch

package object dom extends Implicits with ManagedSubscriptions with SideEffects {

  type VNode = StaticVNode
  type VDomModifier = Modifier

  object VDomModifier {
    val empty: VDomModifier = EmptyModifier

    def apply(modifier: VDomModifier, modifier2: VDomModifier, modifiers: VDomModifier*): VDomModifier = {
      CompositeModifier(Seq(modifier, modifier2) ++ modifiers)
    }

    def apply[T: AsVDomModifier](t: T): VDomModifier = AsVDomModifier[T].asVDomModifier(t)
  }

  type Observable[+A] = monix.reactive.Observable[A]
  val Observable = monix.reactive.Observable

  type Sink[-A] = outwatch.Sink[A]
  val Sink = outwatch.Sink

  type Pipe[-I, +O] = outwatch.Pipe[I, O]
  val Pipe = outwatch.Pipe

  type Handler[T] = outwatch.Handler[T]
  val Handler = outwatch.Handler

  type IO[+T] = cats.effect.IO[T]
  val IO = cats.effect.IO
}

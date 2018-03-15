package outwatch.dom

import cats.effect.{Effect, IO}

trait dsl[F[+_]] extends Styles[F] with Tags[F] with Attributes {

  implicit val effectF: Effect[F]

  type VNode = VNodeF[F]
  type VDomModifier = VDomModifierF[F]

  object tags extends Tags[F] {
    object extra extends TagsExtra[F]
  }
  object attributes extends Attributes {
    object attrs extends Attrs
    object reflected extends ReflectedAttrs
    object props extends Props
    object events extends Events
    object outwatch extends OutwatchAttributes
    object lifecycle extends OutWatchLifeCycleAttributes
  }
  object events {
    object window extends WindowEvents
    object document extends DocumentEvents
  }
}


object dsl extends dsl[IO] with TagsCompat {
  implicit val effectF: Effect[IO] = IO.ioEffect
}
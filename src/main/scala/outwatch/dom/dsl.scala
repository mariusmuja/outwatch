package outwatch.dom

import cats.effect.IO

object dsl extends Styles[IO] with Tags[IO] with Attributes with TagsCompat {
  object tags extends Tags[IO] {
    object extra extends TagsExtra[IO]
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
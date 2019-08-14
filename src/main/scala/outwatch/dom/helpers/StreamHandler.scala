package outwatch.dom.helpers

import outwatch.dom._

import scala.scalajs.js


object StreamHandler {

  private type Updater = js.Array[SimpleModifier] => js.Array[SimpleModifier]

  private def ensureStaticVNodeKeys(mods: SimpleModifiers, key: String): Unit = {
    mods.nodes.indices.foreach { index =>
      mods.nodes(index) match {
        case vtree: VTree => vtree.modifiers.splice(0, 0, Key(key + "." + index))
        case _: StringVNode =>
      }
    }
  }

  // separates modifiers into SimpleModifier(s) and ModifierStream(s)
  private def separateStreams(mods: Seq[Modifier]): (js.Array[SimpleModifier], js.Array[(Int, ModifierStream)]) = {

    // flatten first
    val flattened = js.Array[FlatModifier]()
    flattened.sizeHint(mods.length)

    def flattenHelper(mods: Seq[Modifier]): Unit = {
      mods.foreach {
        case CompositeModifier(inner) => flattenHelper(inner)
        case m: FlatModifier => flattened.push(m)
      }
    }

    flattenHelper(mods)

    // separate
    val modifiers = new js.Array[SimpleModifier](flattened.size)
    val streams = js.Array[(Int, ModifierStream)]()

    flattened.indices.foreach { index =>
      flattened(index) match {
        case m: SimpleModifier =>
          modifiers.update(index, m)
        case m: ModifierStream =>
          modifiers.update(index, EmptyModifier)
          streams.push(index -> m)
      }
    }

    (modifiers, streams)
  }

  private def updaterModifier(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: SimpleModifier => Observable.pure { a => a.update(index, m); a }
    case m: CompositeModifier => updaterCompositeModifier(index, m)
    case m: ModifierStream => updaterModifierStream(index, m)
  }

  private def updaterModifierStream(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater](m => updaterModifier(index, m))
  }

  private def updaterCompositeModifier(index: Int, cm: CompositeModifier): Observable[Updater] = {
    val (modifiers, streams) = separateStreams(cm.modifiers)

    if (streams.isEmpty) {
      Observable.pure { a => a.update(index, SimpleCompositeModifier(modifiers)); a }
    } else {
      val updaters = Observable.fromIterable(streams).mergeMap { case (index, ms) =>
        updaterModifierStream(index, ms)
      }

      updaters.scan(modifiers)((mods, func) => func(mods))
        .prepend(modifiers)
        .map(mods => { a => a.update(index, SimpleCompositeModifier(mods)); a })
    }
  }


  private[outwatch] def from(mods: Seq[Modifier]): SimpleModifiers = {

    val (modifiers, streams) = separateStreams(mods)

    if (streams.isEmpty) {
      SimpleModifiers.from(modifiers)
    } else {
      val streamID = streams.##.toString

      val updaters = Observable.fromIterable(streams).mergeMap { case (index, ms) =>
        updaterModifierStream(index, ms)
      }
      val modifiersStream = updaters.scan(modifiers)((mods, func) => func(mods))
        .map { m => SimpleModifiers.from(m, streamID) }

      // hooks must be appended at the end, original positions can be updated by the streams
      modifiers.push(Lifecycle.lifecycleHooks(modifiersStream): _*)

      val simpleModifiers = SimpleModifiers.from(modifiers, streamID)
      ensureStaticVNodeKeys(simpleModifiers, streamID)

      simpleModifiers
    }
  }
}


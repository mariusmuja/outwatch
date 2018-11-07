package outwatch.dom.helpers

import outwatch.dom._

import scala.scalajs.js

private[outwatch] case class VNodeState(
  initial: SeparatedModifiers,
  stream: Observable[SeparatedModifiers]
) extends SnabbdomState


object VNodeState {

  private type Updater = js.Array[SimpleModifier] => js.Array[SimpleModifier]

  private object KeyGen {
    private var value = KeyGen.##

    def newKey: Key = {
      value += 1
      Key(value)
    }
  }

  private def ensureKeys(mods: js.Array[SimpleModifier]): Unit = {
    var hasKey = false
    mods.indices.foreach { index =>
      mods(index) match {
        case vtree: VTree => mods(index) = vtree.copy(modifiers = KeyGen.newKey +: vtree.modifiers)
        case _: Key => hasKey = true
        case _ =>
      }
    }
    // key must be appended at the end, original positions can be updated by the streams
    if (!hasKey) {
      mods += KeyGen.newKey
      ()
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
        case m: FlatModifier => flattened += m
      }
    }
    flattenHelper(mods)

    // separate
    val modifiers = new js.Array[SimpleModifier](flattened.size)
    val streams = js.Array[(Int, ModifierStream)]()

    flattened.indices.foreach { index =>
      flattened(index) match {
        case m: SimpleModifier => modifiers(index) = m
        case m: ModifierStream =>
          modifiers(index) = EmptyModifier
          streams += index -> m
      }
    }

    // ensure key present for VTrees with stream siblings, as well as for the VTree containing the streams
    if (streams.nonEmpty) ensureKeys(modifiers)

    (modifiers, streams)
  }

  private def updaterM(index: Int, mod: Modifier): Observable[Updater] = mod match {
    case m: SimpleModifier => Observable.pure { a => a.update(index, m); a } // (_.updated(index,m))
    case m: CompositeModifier => updaterCM(index, m)
    case m: ModifierStream => updaterMS(index, m)
  }

  private def updaterMS(index: Int, ms: ModifierStream): Observable[Updater] = {
    ms.stream.switchMap[Updater](m => updaterM(index, m) )
  }

  private def updaterCM(index: Int, cm: CompositeModifier): Observable[Updater] = {
    val (modifiers, streams) = separateStreams(cm.modifiers)

    if (streams.nonEmpty) {
      Observable(streams.map { case (idx, ms) => updaterMS(idx, ms) }: _*).merge
        .scan(modifiers)((mods, func) => func(mods))
        .startWith(Seq(modifiers))
        .map(mods => _.updated(index, SimpleCompositeModifier(mods)))
    }
    else Observable.pure(_.updated(index, SimpleCompositeModifier(modifiers)))
  }

  private[outwatch] def from(mods: Seq[Modifier]): VNodeState = {
    val (modifiers, streams) = separateStreams(mods)

    if (streams.isEmpty) {
      VNodeState(SeparatedModifiers.from(modifiers), Observable.empty)
    } else {

      val modifierStream =
        Observable(streams.map { case (index, ms) => updaterMS(index, ms) }: _*).merge
          .scan(modifiers)((mods, func) => func(mods))
          .map(SeparatedModifiers.from)

      // hooks must be appended at the end, original positions can be updated by the streams
      val modifiersWithLifecycle = modifiers ++ Lifecycle.hooksFor(modifierStream)

      VNodeState(SeparatedModifiers.from(modifiersWithLifecycle), modifierStream)
    }
  }
}


package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom
import outwatch.dom.{DestroyProxyHook, InsertProxyHook, LifecycleHook, Observable}
import snabbdom.{VNodeProxy, patch}

private[outwatch] object Lifecycle {

  def hooksFor(observable: Observable[SeparatedModifiers]): Seq[LifecycleHook] = {

    var cancelable: Option[Cancelable] = None
    val insertHook = InsertProxyHook { (vproxy, scheduler) =>
      implicit val s: Scheduler = scheduler

      def patchProxy(prev: VNodeProxy, modifiers: SeparatedModifiers): VNodeProxy = {
        val t1 = System.nanoTime()
        val proxy = modifiers.toSnabbdom(prev.sel)
        val t2 = System.nanoTime()
        dom.console.log("toSnabbdom time: " + (t2 - t1).toDouble / 1000000)
        val res = patch(prev, proxy)
        val t3 = System.nanoTime()
        dom.console.log("patch time: " + (t3 - t2).toDouble / 1000000)
        res
      }

      if (cancelable.isDefined) {
        dom.console.error("Cancelable subscription already present on insert hook, this is indicative of a bug.")
      }
      cancelable = Some(
        observable.subscribe(
          mods => {
            val newProxy = patchProxy(vproxy, mods)
            vproxy.copyFrom(newProxy)
            Continue
          },
          e => dom.console.error(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
        )
      )
    }

    val destroyHook = DestroyProxyHook { (_, _) =>
      cancelable.foreach(_.cancel())
      cancelable = None
    }

    Seq(insertHook, destroyHook)
  }
}

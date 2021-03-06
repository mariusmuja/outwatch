package outwatch.dom.helpers

import monix.execution.Ack.Continue
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.OverflowStrategy
import org.scalajs.dom
import outwatch.dom.{DestroyLifecycleHook, InsertLifecycleHook, LifecycleHook, Observable}
import snabbdom.{VNodeProxy, patch}

private[outwatch] object Lifecycle {

  def lifecycleHooks(observable: Observable[SimpleModifiers]): Seq[LifecycleHook] = {

    var cancelable: Option[Cancelable] = None
    val insertHook = InsertLifecycleHook { (vproxy, scheduler) =>
      implicit val s: Scheduler = scheduler

      def patchProxy(prev: VNodeProxy, modifiers: SimpleModifiers): VNodeProxy = {
//        val t1 = System.nanoTime()
        val proxy = SnabbdomModifiers.toSnabbdom(prev.sel, modifiers)
//        val t2 = System.nanoTime()
//        dom.console.log("toSnabbdom time: " + (t2 - t1).toDouble / 1000000)
        val res = patch(prev, proxy)
//        val t3 = System.nanoTime()
//        dom.console.log("patch time: " + (t3 - t2).toDouble / 1000000)
        res
      }

      if (cancelable.isDefined) {
        dom.console.error("Cancelable subscription already present on insert hook, this is indicative of a bug.")
      }
      cancelable = Some(
        observable.asyncBoundary(OverflowStrategy.Unbounded).subscribe(
          mods => {
            val newProxy = patchProxy(vproxy, mods)
            vproxy.copyFrom(newProxy)
            Continue
          },
          e => dom.console.error(e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
        )
      )
    }

    val destroyHook = DestroyLifecycleHook { (_, _) =>
      cancelable.foreach(_.cancel())
      cancelable = None
    }

    Seq(insertHook, destroyHook)
  }
}

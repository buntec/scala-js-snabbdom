package com.github.buntec.snabbdom

import org.scalajs.dom
import scalajs.js

class Listener(var vnode: VNode) {

  def handleEvent(event: dom.Event): Unit = {
    val name = event.`type`
    vnode.data.flatMap(_.on).flatMap(_.get(name)).foreach {
      case EventHandler.Single(cb)          => cb(event)
      case EventHandler.SingleWithVNode(cb) => cb(event, vnode)
      case EventHandler.Multi(cbs)          => cbs.foreach(_(event))
      case EventHandler.MultiWithVNode(cbs) => cbs.foreach(_(event, vnode))
    }
  }

  /* This is required because calls to `removeEventListener`
   * need a stable reference to the previously registered
   * listener and using `handleEvent` directly would result
   * in a new implicit conversion to `js.Function1` every time.
   */
  val jsFun: js.Function1[dom.Event, Unit] = handleEvent

}

package com.github.buntec.snabbdom

import org.scalajs.dom
import scalajs.js

class Listener(var vnode: VNode) {

  def handleEvent(event: dom.Event): Unit = {
    val name = event.`type`
    val on = vnode.data.flatMap(_.on)
    on.flatMap(_.get(name)).foreach { cb => cb(event) }
  }

  /* This is required because calls to `removeEventListener`
   * need a stable reference to the previously registered
   * listener and using `handleEvent` directly would result
   * in a new implicit conversion to `js.Function1` every time.
   */
  val jsFun: js.Function1[dom.Event, Unit] = handleEvent

}

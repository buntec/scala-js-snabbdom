package com.github.buntec.snabbdom

import org.scalajs.dom

class Listener(var vnode: VNode) {

  def handleEvent(event: dom.Event): Unit = {
    val name = event.`type`
    val on = vnode.data.flatMap(_.on)
    on.flatMap(_.get(name)).foreach { cb => cb(event) }
  }
}

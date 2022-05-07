package com.github.buntec.snabbdom

import org.scalajs.dom

sealed trait EventHandler

object EventHandler {

  def apply(cbs: (dom.Event => Unit)*): EventHandler = Multi(cbs)
  def usingVNode(cbs: ((dom.Event, VNode) => Unit)*): EventHandler =
    MultiWithVNode(cbs)

  // Not sure about this...
  implicit def fromFunction1(cb: dom.Event => Unit): EventHandler = apply(cb)
  implicit def fromFunction2(cb: (dom.Event, VNode) => Unit): EventHandler =
    usingVNode(cb)

  case class Single private (cb: dom.Event => Unit) extends EventHandler
  case class SingleWithVNode private (cb: (dom.Event, VNode) => Unit)
      extends EventHandler
  case class Multi private (cbs: Seq[dom.Event => Unit]) extends EventHandler
  case class MultiWithVNode private (cbs: Seq[(dom.Event, VNode) => Unit])
      extends EventHandler

}

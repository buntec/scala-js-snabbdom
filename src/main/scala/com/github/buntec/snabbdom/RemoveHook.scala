package com.github.buntec.snabbdom

trait RemoveHook {

  def apply(vNode: VNode, removeCallback: () => Unit): Any

}

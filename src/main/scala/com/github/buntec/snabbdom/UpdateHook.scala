package com.github.buntec.snabbdom

trait UpdateHook {

  def apply(oldVNode: VNode, vNode: VNode): Any

}

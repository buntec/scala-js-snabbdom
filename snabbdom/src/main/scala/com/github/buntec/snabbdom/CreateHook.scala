package com.github.buntec.snabbdom

trait CreateHook {

  def apply(emptyVNode: VNode, vNode: VNode): Any

}

package com.github.buntec.snabbdom

trait PrePatchHook {

  def apply(oldVNode: VNode, vNode: VNode): Any

}

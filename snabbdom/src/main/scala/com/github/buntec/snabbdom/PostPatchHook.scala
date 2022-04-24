package com.github.buntec.snabbdom

trait PostPatchHook {

  def apply(oldVNode: VNode, vNode: VNode): Any

}

package com.github.buntec.snabbdom

trait DestroyHook {

  def apply(vNode: VNode): Any

}

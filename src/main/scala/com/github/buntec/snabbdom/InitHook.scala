package com.github.buntec.snabbdom

trait InitHook {

  def apply(vNode: VNode): Any
}

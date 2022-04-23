package com.github.buntec.snabbdom

trait InsertHook {

  def apply(vNode: VNode): Any

}

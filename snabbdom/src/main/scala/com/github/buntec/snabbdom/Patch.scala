package com.github.buntec.snabbdom

import org.scalajs.dom

trait Patch {

  def apply(oldVnode: VNode, vnode: VNode): VNode

  def apply(elm: dom.Element, vnode: VNode): VNode

  def apply(frag: dom.DocumentFragment, vnode: VNode): VNode

}

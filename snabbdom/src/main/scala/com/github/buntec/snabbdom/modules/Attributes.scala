package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom.Module
import com.github.buntec.snabbdom._
import org.scalajs.dom

object Attributes {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateAttrs(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateAttrs(oldVNode, vNode)
    })
  )

  private val xlinkNS = "http://www.w3.org/1999/xlink"
  private val xmlNS = "http://www.w3.org/XML/1998/namespace"
  private val colonChar = 58
  private val xChar = 120

  private def updateAttrs(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.elm.get.asInstanceOf[dom.Element]

    def update(
        oldAttrs: Map[String, AttrValue],
        attrs: Map[String, AttrValue]
    ) = {
      attrs.foreach { case (key, cur) =>
        val old = oldAttrs.get(key)
        if (old.forall(_ != cur)) {
          if (cur == true) {
            elm.setAttribute(key, "")
          } else if (cur == false) {
            elm.removeAttribute(key)
          } else {
            if (key.codePointAt(0) != xChar) {
              elm.setAttribute(key, cur.toString)
            } else if (key.codePointAt(3) == colonChar) {
              elm.setAttributeNS(xmlNS, key, cur.toString)
            } else if (key.codePointAt(5) == colonChar) {
              elm.setAttributeNS(xlinkNS, key, cur.toString)
            } else {
              elm.setAttribute(key, cur.toString)
            }
          }
        }
      }

      oldAttrs.foreach { case (key, _) =>
        if (!attrs.contains(key)) {
          elm.removeAttribute(key)
        }
      }
    }

    val oldAttrs = oldVnode.data.flatMap(_.attrs)
    val attrs = vnode.data.flatMap(_.attrs)

    (oldAttrs, attrs) match {
      case (Some(oldAttrs), Some(attrs)) if (oldAttrs != attrs) =>
        update(oldAttrs, attrs)
      case (Some(oldAttrs), None) =>
        update(oldAttrs, Map.empty)
      case (None, Some(attrs)) =>
        update(Map.empty, attrs)
      case _ => ()
    }

  }

}

package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom.Module
import com.github.buntec.snabbdom._
import scalajs.js
import org.scalajs.dom

object Styles {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateStyle(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateStyle(oldVNode, vNode)
    })
  )

  private def updateStyle(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.elm.get

    def update(
        oldStyle: Map[String, StyleValue],
        style: Map[String, StyleValue]
    ): Unit = {

      oldStyle.foreach { case (name, _) =>
        style.get(name) match {
          case Some(_) =>
          case None =>
            if (name(0) == '-' && name(1) == '-') {
              elm.asInstanceOf[dom.HTMLElement].style.removeProperty(name)
            } else {
              elm
                .asInstanceOf[dom.HTMLElement]
                .style
                .asInstanceOf[js.Dictionary[String]](name) = ""
            }
        }
      }

      style.foreach { case (name, cur) =>
        if (oldStyle.get(name).forall(_ != cur)) {

          if (name(0) == '-' && name(1) == '-') {

            elm.asInstanceOf[dom.HTMLElement].style.setProperty(name, cur)

          } else {

            elm
              .asInstanceOf[dom.HTMLElement]
              .style
              .asInstanceOf[js.Dictionary[String]](name) = cur

          }
        }
      }

    }

    (oldVnode.data.flatMap(_.style), vnode.data.flatMap(_.style)) match {
      case (Some(oldStyle), Some(style)) if oldStyle != style =>
        update(oldStyle, style)
      case (Some(oldStyle), None) => update(oldStyle, Map.empty)
      case (None, Some(style))    => update(Map.empty, style)
      case _                      => ()
    }

  }

}

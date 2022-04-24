package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom.Module
import com.github.buntec.snabbdom._
import org.scalajs.dom

object Classes {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateClasses(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateClasses(oldVNode, vNode)
    })
  )

  private def updateClasses(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.elm.get.asInstanceOf[dom.Element]

    def update(
        oldClass: Map[String, Boolean],
        klass: Map[String, Boolean]
    ): Unit = {
      oldClass.foreach { case (name, flag) =>
        if (flag && !klass.contains(name)) {
          elm.classList.remove(name)
        }
      }
      klass.foreach { case (name, cur) =>
        if (oldClass.get(name).forall(_ != cur)) {
          if (cur) {
            elm.classList.add(name)
          } else {
            elm.classList.remove(name)
          }
        }
      }
    }

    (oldVnode.data.flatMap(_.`class`), vnode.data.flatMap(_.`class`)) match {
      case (Some(oldClass), Some(klass)) if oldClass != klass =>
        update(oldClass, klass)
      case (Some(oldClass), None) => update(oldClass, Map.empty)
      case (None, Some(klass))    => update(Map.empty, klass)
      case _                      => ()
    }

  }

}

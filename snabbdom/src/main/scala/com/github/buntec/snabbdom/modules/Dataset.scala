package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom._
import org.scalajs.dom

object Dataset {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateDataset(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateDataset(oldVNode, vNode)
    })
  )

  private val CAPS_REGEX = "[A-Z]"

  private def updateDataset(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.asInstanceOf[dom.HTMLElement]
    val oldDataset = oldVnode.data.flatMap(_.dataset)
    val dataset = vnode.data.flatMap(_.dataset)
    val d = elm.dataset

    def update(
        oldDataset: Map[String, String],
        dataset: Map[String, String]
    ): Unit = {

      oldDataset.foreach { case (key, _) =>
        dataset.get(key) match {
          case None =>
            if (d != null) { // TODO: does this make sense?
              d -= key
            } else {
              elm.removeAttribute(
                "data-" + key.replaceAll(CAPS_REGEX, "-$&").toLowerCase()
              )
            }
          case Some(_) => ()
        }
      }

      dataset.foreach { case (key, value) =>
        if (oldDataset.get(key).forall(_ != value)) {
          if (d != null) { // TODO: does this make sense?
            d += (key -> value)
          } else {
            elm.setAttribute(
              "data-" + key.replaceAll(CAPS_REGEX, "-$&").toLowerCase(),
              value
            )
          }
        }
      }

    }

    (oldDataset, dataset) match {
      case (Some(oldDataset), Some(dataset)) if oldDataset != dataset =>
        update(oldDataset, dataset)
      case (Some(oldDataset), None) =>
        update(oldDataset, Map.empty)
      case (None, Some(dataset)) => update(Map.empty, dataset)
      case _                     => ()
    }

  }

}

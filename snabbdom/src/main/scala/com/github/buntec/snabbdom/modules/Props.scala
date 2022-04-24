package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom._
import scalajs.js

object Props {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateProps(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateProps(oldVNode, vNode)
    })
  )

  private def updateProps(oldVnode: VNode, vnode: VNode): Unit = {
    val elm = vnode.elm.get
    val oldProps = oldVnode.data.flatMap(_.props)
    val props = vnode.data.flatMap(_.props)

    def update(
        oldProps: Map[String, PropValue],
        props: Map[String, PropValue]
    ): Unit = {
      props.foreach { case (key, cur) =>
        if (
          oldProps.get(key).forall(_ != cur) && (key != "value" || elm
            .asInstanceOf[js.Dictionary[Any]](key) != cur)
        ) { elm.asInstanceOf[js.Dictionary[Any]](key) = cur }
      }
    }

    (oldProps, props) match {
      case (Some(oldProps), Some(props)) if oldProps != props =>
        update(oldProps, props)
      case (None, Some(props)) => update(Map.empty, props)
      case _                   => ()
    }

  }

}

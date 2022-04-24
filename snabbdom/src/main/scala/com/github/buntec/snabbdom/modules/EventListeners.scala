package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom._
import org.scalajs.dom

object EventListeners {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateEventListeners(emptyVNode, Some(vNode))
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateEventListeners(oldVNode, Some(vNode))
    }),
    destroy = Some(new DestroyHook {
      override def apply(vnode: VNode): Any =
        updateEventListeners(vnode, None)
    })
  )

  private def invokeHandler(
      handler: dom.Event => Unit,
      vnode: VNode, // TODO
      event: Option[dom.Event]
  ): Unit = {
    event.foreach(handler) // TODO:  compare with JS snabbdom
  }

  private def handleEvent(event: dom.Event, vnode: VNode) = {
    val name = event.`type`
    val on = vnode.data.flatMap(_.on)

    on.foreach { on =>
      on.get(name) match {
        case Some(cb) => invokeHandler(cb, vnode, Some(event))
        case None     => ()
      }
    }
  }

  private def createListener(vnode: VNode) = { (event: dom.Event) =>
    handleEvent(event, vnode)
  }

  private def updateEventListeners(
      oldVnode: VNode,
      vnode: Option[VNode]
  ): Unit = {

    val oldOn = oldVnode.data.flatMap(_.on)
    val oldListener = oldVnode.listener
    val oldElm = oldVnode.elm.map(_.asInstanceOf[dom.Element])
    val on = vnode.flatMap(_.data).flatMap(_.on)
    val elm = vnode.flatMap(_.elm).map(_.asInstanceOf[dom.Element])

    (oldOn, oldListener, on) match {
      case (None, _, None)                           => ()
      case (Some(_), None, None)                     => ()
      case (Some(oldOn), _, Some(on)) if oldOn == on => ()
      case (Some(oldOn), Some(oldListener), Some(on)) =>
        oldOn.foreach { case (name, _) =>
          if (on.get(name).isEmpty) {
            oldElm.foreach(_.removeEventListener(name, oldListener, false))
          }
        }

        val listener = oldVnode.listener.getOrElse(createListener(vnode.get))
        vnode.foreach(_.listener = Some(listener))

        on.foreach { case (name, _) =>
          if (!oldOn.contains(name)) {
            elm.foreach(_.addEventListener(name, listener, false))
          }
        }

      case (Some(oldOn), None, Some(on)) =>
        val listener = oldVnode.listener.getOrElse(createListener(vnode.get))
        vnode.foreach(_.listener = Some(listener))

        on.foreach { case (name, _) =>
          if (!oldOn.contains(name)) {
            elm.foreach(_.addEventListener(name, listener, false))
          }
        }

      case (Some(oldOn), Some(oldListener), None) =>
        oldOn.foreach { case (name, _) =>
          oldElm.foreach(_.removeEventListener(name, oldListener, false))
        }

      case (None, _, Some(on)) =>
        val listener = oldVnode.listener.getOrElse(createListener(vnode.get))
        vnode.foreach(_.listener = Some(listener))
        on.foreach { case (name, _) =>
          elm.foreach(_.addEventListener(name, listener, false))
        }

    }

  }

}

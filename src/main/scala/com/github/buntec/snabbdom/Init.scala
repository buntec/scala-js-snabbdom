package com.github.buntec.snabbdom

import org.scalajs.dom

object Init {

  val emptyNode = VNode.create(Some(""), None, None, None, None)

  def apply(
      modules: Seq[Module],
      domApi: Option[DomApi]
  ) = {

    val api = domApi.getOrElse(DomApi.apply)

    val cbs: ModuleHooks = ???

    def emptyNodeAt(elm: dom.Element): VNode = {
      val id = Option(elm.id).map("#" + _).getOrElse("")
      val classes = Option(elm.getAttribute("class"))
      val c = classes.map("." + _.split(" ").mkString(".")).getOrElse("")

      VNode.create(
        Some(api.tagName(elm).toLowerCase() + id + c),
        None,
        None,
        None,
        Some(elm)
      )

    }

    def emptyDocumentFragmentAt(frag: dom.DocumentFragment): VNode = {
      VNode.create(None, None, None, None, Some(frag))
    }

    def createRmCb(childElm: dom.Node, listeners: Int): () => Unit = { () =>
      {
        if (listeners == 1) {
          val parent = api.parentNode(childElm)
          parent.foreach(node => api.removeChild(node, childElm))
        }
      }
    }

    def createElm(vnode: VNode, insertedVNodeQueue: VNodeQueue): dom.Node = {

      var data = vnode.data

      for {
        d <- data
        h <- d.hook
        init0 <- h.init
      } yield {
        init0(vnode)
        data = vnode.data
      }

      val sel = vnode.sel
      sel match {
        case Some("!") =>
          vnode.text = Some(vnode.text.getOrElse(""))
          vnode.elm = Some(api.createComment(vnode.text.get))

        case Some(sel) =>
          val hashIdx = sel.indexOf("#")
          val dotIdx = sel.indexOf(".", hashIdx)
          val hash = if (hashIdx > 0) hashIdx else sel.length
          val dot = if (dotIdx > 0) dotIdx else sel.length
          val tag = if (hashIdx != -1 || dotIdx != -1) {
            sel.slice(0, math.min(hash, dot))
          } else {
            sel
          }
          val elm = if (data.isDefined && data.exists(_.ns.isDefined)) {
            api.createElementNS(
              data.flatMap(_.ns).get,
              tag
            ) // TODO what about data?
          } else {
            api.createElement(tag) // TODO what about data argument?
          }
          vnode.elm = Some(elm)
          if (hash < dot) elm.setAttribute("id", sel.slice(hash + 1, dot))
          if (dotIdx > 0) {
            elm.setAttribute(
              "class",
              sel.slice(dot + 1, sel.length).replaceAll("""\./g""", " ")
            )
          }
          cbs.create.foreach(_.apply(emptyNode, vnode))
          vnode.children match {
            case None =>
              vnode.text match {
                case None => ()
                case Some(text) =>
                  api.appendChild(elm, api.createTextNode(text))
              }
            case Some(children) =>
              children.foreach { child =>
                if (child != null) { // TODO: is this necessary
                  api.appendChild(elm, createElm(child, insertedVNodeQueue))
                }
              }
          }
          vnode.data.flatMap(_.hook).map { hooks =>
            hooks.create.foreach(hook => hook(emptyNode, vnode))
            hooks.insert.foreach { _ => insertedVNodeQueue.append(vnode) }
          }

        case None =>
          vnode.children match {
            case None => vnode.elm = Some(api.createTextNode(vnode.text.get))
            case Some(children) =>
              val elm = api.createDocumentFragment
              vnode.elm = Some(elm)
              cbs.create.foreach(hook => hook(emptyNode, vnode))
              children.foreach { child =>
                if (child != null) {
                  api.appendChild(
                    elm,
                    createElm(child, insertedVNodeQueue)
                  )
                }
              }
          }

      }

      vnode.elm.get

    }

    def addVnodes(
        parentElm: dom.Node,
        before: Option[dom.Node],
        vnodes: Array[VNode],
        startIdx: Int,
        endIdx: Int,
        insertedVNodeQueue: VNodeQueue
    ): Unit = {
      var i = startIdx
      while (i <= endIdx) {
        i += 1
        val ch = vnodes(i)
        if (ch != null) { // TODO: is this necessary?
          api.insertBefore(parentElm, createElm(ch, insertedVNodeQueue), before)
        }
      }
    }

    def invokeDestroyHook(vnode: VNode): Unit = {
      vnode.data.foreach { data =>
        data.hook.flatMap(_.destroy).foreach(hook => hook(vnode))
        cbs.destroy.foreach(hook => hook(vnode))
        vnode.children.foreach {
          _.foreach { child =>
            if (child != null) { // TODO: is this necessary?
              invokeDestroyHook(child)
            }
          }
        }
      }
    }

    def removeVNodes(
        parentElm: dom.Node,
        vnodes: Array[VNode],
        startIdx: Int,
        endIdx: Int
    ): Unit = {

      var i = startIdx
      while (i <= endIdx) {
        i += 1
        val ch = vnodes(i)
        if (ch != null) { // TODO: is this necessary?
          ch.sel match {
            case Some(_) =>
              invokeDestroyHook(ch)
              val listeners = cbs.remove.length + 1
              val rm = createRmCb(ch.elm.get, listeners)
              cbs.remove.foreach(hook => hook(ch, rm))
              ch.data
                .flatMap(_.hook)
                .flatMap(_.remove)
                .fold(rm()) { hook => hook(ch, rm); () }
            case None => // text node
              api.removeChild(parentElm, ch.elm.get)

          }
        }
      }
    }

  }

}

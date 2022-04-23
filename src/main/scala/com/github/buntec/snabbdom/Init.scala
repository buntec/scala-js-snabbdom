package com.github.buntec.snabbdom

import org.scalajs.dom

object Init {

  val emptyNode = VNode.create(Some(""), None, None, None, None)

  def sameVnode(vnode1: VNode, vnode2: VNode): Boolean = {
    vnode1.key == vnode2.key && vnode1.data.map(_.is) == vnode2.data.map(_.is)
  }

  def createKeyToOldIdx(
      children: Array[VNode],
      beginIdx: Int,
      endIdx: Int
  ): Map[String, Int] = {
    children.zipWithIndex
      .map { case (ch, i) =>
        ch.key.map { key => (key.asInstanceOf[String] -> i) }
      }
      .collect { case Some(a) => a }
      .toMap
      .filter(kv => kv._2 >= beginIdx && kv._2 <= endIdx)
  }

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

    def removeVnodes(
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

    def updateChildren(
        parentElm: dom.Node,
        oldCh: Array[VNode],
        newCh: Array[VNode],
        insertedVnodeQueue: VNodeQueue
    ): Unit = {

      var oldStartIdx = 0
      var newStartIdx = 0
      var oldEndIdx = oldCh.length - 1
      var oldStartVnode = oldCh(0)
      var oldEndVnode = oldCh(oldEndIdx)
      var newEndIdx = newCh.length - 1
      var newStartVnode = newCh(0)
      var newEndVnode = newCh(newEndIdx)

      var oldKeyToIdx: Map[String, Int] = null
      var elmToMove: VNode = null

      while (oldStartIdx <= oldEndIdx && newStartIdx <= newEndIdx) {
        if (oldStartVnode == null) {
          oldStartIdx += 1
          oldStartVnode = oldCh(oldStartIdx) // Vnode might have been moved left
        } else if (oldEndVnode == null) {
          oldEndIdx -= 1
          oldEndVnode = oldCh(oldEndIdx)
        } else if (newStartVnode == null) {
          newStartIdx += 1
          newStartVnode = newCh(newStartIdx)
        } else if (newEndVnode == null) {
          newEndIdx -= 1
          newEndVnode = newCh(newEndIdx)
        } else if (sameVnode(oldStartVnode, newStartVnode)) {
          patchVnode(oldStartVnode, newStartVnode, insertedVnodeQueue)
          oldStartIdx += 1
          oldStartVnode = oldCh(oldStartIdx)
          newStartIdx += 1
          newStartVnode = newCh(newStartIdx)
        } else if (sameVnode(oldEndVnode, newEndVnode)) {
          patchVnode(oldEndVnode, newEndVnode, insertedVnodeQueue)
          oldEndIdx -= 1
          oldEndVnode = oldCh(oldEndIdx)
          newEndIdx -= 1
          newEndVnode = newCh(newEndIdx)
        } else if (sameVnode(oldStartVnode, newEndVnode)) {
          // Vnode moved right
          patchVnode(oldStartVnode, newEndVnode, insertedVnodeQueue)
          api.insertBefore(
            parentElm,
            oldStartVnode.elm.get,
            api.nextSibling(oldEndVnode.elm.get)
          )
          oldStartIdx += 1
          oldStartVnode = oldCh(oldStartIdx)
          newEndIdx -= 1
          newEndVnode = newCh(newEndIdx)
        } else if (sameVnode(oldEndVnode, newStartVnode)) {
          // Vnode moved left
          patchVnode(oldEndVnode, newStartVnode, insertedVnodeQueue)
          api.insertBefore(
            parentElm,
            oldEndVnode.elm.get,
            oldStartVnode.elm
          )
          oldEndIdx -= 1
          oldEndVnode = oldCh(oldEndIdx)
          newStartIdx += 1
          newStartVnode = newCh(newStartIdx)
        } else {
          if (oldKeyToIdx == null) {
            oldKeyToIdx = createKeyToOldIdx(oldCh, oldStartIdx, oldEndIdx)
          }
          val idxInOld =
            oldKeyToIdx.get(newStartVnode.key.asInstanceOf[String])
          idxInOld match {
            case None =>
              // New element
              api.insertBefore(
                parentElm,
                createElm(newStartVnode, insertedVnodeQueue),
                oldStartVnode.elm
              )
            case Some(idxInOld) =>
              elmToMove = oldCh(idxInOld)
              if (elmToMove.sel != newStartVnode.sel) {
                api.insertBefore(
                  parentElm,
                  createElm(newStartVnode, insertedVnodeQueue),
                  oldStartVnode.elm
                )
              } else {
                patchVnode(elmToMove, newStartVnode, insertedVnodeQueue)
                oldCh(idxInOld) = null // undefined as any
                api.insertBefore(
                  parentElm,
                  elmToMove.elm.get,
                  oldStartVnode.elm
                )
              }
          }
          newStartIdx += 1
          newStartVnode = newCh(newStartIdx)
        }
      }

      if (newStartIdx <= newEndIdx) {
        val before = Option(newCh(newEndIdx + 1)).flatMap(_.elm)
        addVnodes(
          parentElm,
          before,
          newCh,
          newStartIdx,
          newEndIdx,
          insertedVnodeQueue
        )
      }
      if (oldStartIdx <= oldEndIdx) {
        removeVnodes(parentElm, oldCh, oldStartIdx, oldEndIdx)
      }

    }

    def patchVnode(
        oldVnode: VNode,
        vnode: VNode,
        insertedVNodeQueue: VNodeQueue
    ): Unit = {
      val hook = vnode.data.flatMap(_.hook)
      hook.flatMap(_.prepatch).foreach(hook => hook(oldVnode, vnode))
      val elm = oldVnode.elm.get
      vnode.elm = Some(elm)
      val oldCh = oldVnode.children
      val ch = vnode.children
      if (oldVnode == vnode) {
        () // done
      } else {

        vnode.data.foreach { data =>
          cbs.update.foreach(hook => hook(oldVnode, vnode))
          vnode.data
            .flatMap(_.hook)
            .flatMap(_.update)
            .foreach(hook => hook(oldVnode, vnode))
        }

      }

    }

  }

}

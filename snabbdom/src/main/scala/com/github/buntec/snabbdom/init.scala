/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Copyright (c) 2015 Simon Friis Vindum
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.github.buntec.snabbdom

import org.scalajs.dom
import scala.collection.mutable

import com.github.buntec.snabbdom.modules._

object init {

  def apply: Patch = {
    apply(
      Seq(
        Attributes.module,
        Classes.module,
        Props.module,
        Styles.module,
        EventListeners.module,
        Dataset.module
      )
    )
  }

  def apply(
      modules: Seq[Module],
      domApi: Option[DomApi] = None
  ): Patch = {

    val api = domApi.getOrElse(DomApi.apply)

    val cbs: ModuleHooks = modules.foldLeft(ModuleHooks.empty) {
      case (hooks, module) =>
        hooks.copy(
          create = module.create.fold(hooks.create)(_ :: hooks.create),
          update = module.update.fold(hooks.update)(_ :: hooks.update),
          remove = module.remove.fold(hooks.remove)(_ :: hooks.remove),
          destroy = module.destroy.fold(hooks.destroy)(_ :: hooks.destroy),
          pre = module.pre.fold(hooks.pre)(_ :: hooks.pre),
          post = module.post.fold(hooks.post)(_ :: hooks.post)
        )
    }

    def emptyNodeAt(elm: dom.Element): VNode = {
      val id = Option(elm.id).map("#" + _).getOrElse("")
      val classes = Option(elm.getAttribute("class"))
      val c = classes.map("." + _.split(" ").mkString(".")).getOrElse("")

      VNode.create(
        Some(api.tagName(elm).toLowerCase + id + c),
        None,
        None,
        None,
        Some(elm)
      )

    }

    def emptyDocumentFragmentAt(frag: dom.DocumentFragment): VNode = {
      VNode.create(None, None, None, None, Some(frag))
    }

    def createRmCb(childElm: dom.Node, listeners: Int): () => Unit = {
      var listeners0 = listeners
      () => {
        listeners0 -= 1
        if (listeners0 == 0) {
          val parent = api.parentNode(childElm)
          parent.foreach(parent => api.removeChild(parent, childElm))
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
              sel.slice(dot + 1, sel.length).replaceAll("""\.""", " ")
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
        val ch = vnodes(i)
        if (ch != null) { // TODO: is this necessary?
          api.insertBefore(parentElm, createElm(ch, insertedVNodeQueue), before)
        }
        i += 1
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
        i += 1
      }
    }

    def getOrNull(vnodes: Array[VNode], idx: Int): VNode = {
      if (idx >= 0 && idx < vnodes.length) {
        vnodes(idx)
      } else {
        null
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
      var oldStartVnode = getOrNull(oldCh, 0)
      var oldEndVnode = getOrNull(oldCh, oldEndIdx)
      var newEndIdx = newCh.length - 1
      var newStartVnode = getOrNull(newCh, 0)
      var newEndVnode = getOrNull(newCh, newEndIdx)

      var oldKeyToIdx: Map[String, Int] = null
      var elmToMove: VNode = null

      while (oldStartIdx <= oldEndIdx && newStartIdx <= newEndIdx) {
        if (oldStartVnode == null) {
          oldStartIdx += 1
          oldStartVnode =
            getOrNull(oldCh, oldStartIdx) // Vnode might have been moved left
        } else if (oldEndVnode == null) {
          oldEndIdx -= 1
          oldEndVnode = getOrNull(oldCh, oldEndIdx)
        } else if (newStartVnode == null) {
          newStartIdx += 1
          newStartVnode = getOrNull(newCh, newStartIdx)
        } else if (newEndVnode == null) {
          newEndIdx -= 1
          newEndVnode = getOrNull(newCh, newEndIdx)
        } else if (sameVnode(oldStartVnode, newStartVnode)) {
          patchVnode(oldStartVnode, newStartVnode, insertedVnodeQueue)
          oldStartIdx += 1
          oldStartVnode = getOrNull(oldCh, oldStartIdx)
          newStartIdx += 1
          newStartVnode = getOrNull(newCh, newStartIdx)
        } else if (sameVnode(oldEndVnode, newEndVnode)) {
          patchVnode(oldEndVnode, newEndVnode, insertedVnodeQueue)
          oldEndIdx -= 1
          oldEndVnode = getOrNull(oldCh, oldEndIdx)
          newEndIdx -= 1
          newEndVnode = getOrNull(newCh, newEndIdx)
        } else if (sameVnode(oldStartVnode, newEndVnode)) {
          // Vnode moved right
          patchVnode(oldStartVnode, newEndVnode, insertedVnodeQueue)
          api.insertBefore(
            parentElm,
            oldStartVnode.elm.get,
            api.nextSibling(oldEndVnode.elm.get)
          )
          oldStartIdx += 1
          oldStartVnode = getOrNull(oldCh, oldStartIdx)
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
          oldEndVnode = getOrNull(oldCh, oldEndIdx)
          newStartIdx += 1
          newStartVnode = getOrNull(newCh, newStartIdx)
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
          newStartVnode = getOrNull(newCh, newStartIdx)
        }
      }

      if (newStartIdx <= newEndIdx) {
        val before = Option(getOrNull(newCh, newEndIdx + 1)).flatMap(_.elm)
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

      if (oldVnode != vnode) {

        vnode.data.foreach { _ =>
          cbs.update.foreach(hook => hook(oldVnode, vnode))
          vnode.data
            .flatMap(_.hook)
            .flatMap(_.update)
            .foreach(hook => hook(oldVnode, vnode))
        }

        vnode.text match {
          case None =>
            (oldCh, ch) match {
              case (Some(oldCh), Some(ch)) =>
                if (oldCh != ch) {
                  updateChildren(elm, oldCh, ch, insertedVNodeQueue)
                }
              case (None, Some(ch)) =>
                oldVnode.text.foreach(_ => api.setTextContext(elm, Some("")))
                addVnodes(elm, None, ch, 0, ch.length - 1, insertedVNodeQueue)
              case (Some(oldCh), None) =>
                removeVnodes(elm, oldCh, 0, oldCh.length - 1)
              case (None, None) =>
                oldVnode.text.foreach(_ => api.setTextContext(elm, Some("")))
            }
          case Some(text) if oldVnode.text.forall(_ != text) =>
            oldCh.foreach(oldChildren =>
              removeVnodes(elm, oldChildren, 0, oldChildren.length - 1)
            )
            api.setTextContext(elm, Some(text))
          case Some(_) => ()
        }

        hook.flatMap(_.postpatch).foreach(hook => hook(oldVnode, vnode))

      }
    }

    def patch(oldVnode: VNode, vnode: VNode): VNode = {

      val insertedVNodeQueue: VNodeQueue = mutable.ArrayBuffer.empty[VNode]

      if (sameVnode(oldVnode, vnode)) {
        patchVnode(oldVnode, vnode, insertedVNodeQueue)
      } else {
        val elm = oldVnode.elm.get
        val parent = api.parentNode(elm)
        createElm(vnode, insertedVNodeQueue)
        parent match {
          case Some(parent) =>
            api.insertBefore(parent, vnode.elm.get, api.nextSibling(elm))
            removeVnodes(parent, Array(oldVnode), 0, 0)
          case None => ()
        }
      }

      insertedVNodeQueue.foreach(vnode =>
        vnode.data
          .flatMap(_.hook)
          .flatMap(_.insert)
          .foreach(hook => hook(vnode))
      )

      cbs.post.foreach(hook => hook())

      vnode

    }

    new Patch {

      override def apply(oldVnode: VNode, vnode: VNode): VNode =
        patch(oldVnode, vnode)

      override def apply(elm: dom.Element, vnode: VNode): VNode =
        patch(emptyNodeAt(elm), vnode)

      override def apply(frag: dom.DocumentFragment, vnode: VNode): VNode =
        patch(emptyDocumentFragmentAt(frag), vnode)

    }

  }

  private val emptyNode = VNode.create(Some(""), None, None, None, None)

  private def sameVnode(vnode1: VNode, vnode2: VNode): Boolean = {
    vnode1.key == vnode2.key &&
    vnode1.data.flatMap(_.is) == vnode2.data.flatMap(_.is) &&
    vnode1.sel == vnode2.sel
  }

  private def createKeyToOldIdx(
      children: Array[VNode],
      beginIdx: Int,
      endIdx: Int
  ): Map[String, Int] = {
    children.zipWithIndex
      .map { case (ch, i) =>
        ch.key.map { key => (key -> i) }
      }
      .collect { case Some(a) => a }
      .toMap
      .filter(kv => kv._2 >= beginIdx && kv._2 <= endIdx)
  }

}

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

package snabbdom

import org.scalajs.dom
import scala.collection.mutable

import snabbdom.modules._

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

    type VNodeQueue = mutable.ArrayBuffer[VNode]

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
      val id = Option(elm.id).filter(_.nonEmpty).fold("")("#" + _)
      val classes = Option(elm.getAttribute("class"))
      val c = classes.map("." + _.split(" ").mkString(".")).getOrElse("")

      VNode.create(
        Some(api.tagName(elm).toLowerCase + id + c),
        VNodeData.empty,
        None,
        None,
        Some(elm)
      )

    }

    def emptyDocumentFragmentAt(frag: dom.DocumentFragment): VNode = {
      VNode.create(None, VNodeData.empty, None, None, Some(frag))
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

    def createElm(vnode: VNode, insertedVNodeQueue: VNodeQueue): VNode = {

      var data = vnode.data

      for {
        h <- data.hook
        init0 <- h.init
      } yield {
        init0(vnode)
        data = vnode.data
      }

      val sel = vnode.sel
      sel match {
        case Some("!") =>
          vnode.copy(
            text = Some(vnode.text.getOrElse("")),
            elm = Some(api.createComment(vnode.text.get))
          )

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
          val elm = if (data.ns.isDefined) {
            api.createElementNS(
              data.ns.get,
              tag
            ) // TODO what about data?
          } else {
            api.createElement(tag) // TODO what about data argument?
          }
          val vnode0 = vnode.copy(
            elm = Some(elm),
            children =
              vnode.children.map(_.map(ch => createElm(ch, insertedVNodeQueue)))
          )
          if (hash < dot) elm.setAttribute("id", sel.slice(hash + 1, dot))
          if (dotIdx > 0) {
            elm.setAttribute(
              "class",
              sel.slice(dot + 1, sel.length).replaceAll("""\.""", " ")
            )
          }
          cbs.create.foreach(_.apply(vnode0))
          vnode0.children match {
            case None =>
              vnode0.text match {
                case None => ()
                case Some(text) =>
                  api.appendChild(elm, api.createTextNode(text))
              }
            case Some(children) =>
              children.foreach { child =>
                api.appendChild(elm, child.elm.get)
              }
          }
          vnode0.data.hook.map { hooks =>
            hooks.create.foreach(hook => hook(vnode))
            hooks.insert.foreach { _ => insertedVNodeQueue.append(vnode) }
          }
          vnode0

        case None =>
          vnode.children match {
            case None =>
              vnode.copy(elm =
                Some(api.createTextNode(vnode.text.getOrElse("")))
              )
            case Some(children) =>
              val elm = api.createDocumentFragment
              val vnode0 = vnode.copy(
                elm = Some(elm),
                children =
                  Some(children.map(ch => createElm(ch, insertedVNodeQueue)))
              )
              cbs.create.foreach(hook => hook(vnode0))
              vnode0.children.foreach { children =>
                children.foreach(ch =>
                  api.appendChild(
                    elm,
                    ch.elm.get
                  )
                )
              }
              vnode0
          }

      }

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
        vnodes(i) = createElm(vnodes(i), insertedVNodeQueue)
        api.insertBefore(
          parentElm,
          vnodes(i).elm.get,
          before
        )
        i += 1
      }
    }

    def invokeDestroyHook(vnode: VNode): Unit = {
      if (!vnode.isTextNode) { // detroy hooks should not be called on text nodes
        vnode.data.hook.flatMap(_.destroy).foreach(hook => hook(vnode))
        cbs.destroy.foreach(hook => hook(vnode))
        vnode.children.foreach {
          _.foreach { child =>
            invokeDestroyHook(child)
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
        ch.sel match {
          case Some(_) =>
            invokeDestroyHook(ch)
            val listeners = cbs.remove.length + 1
            val rm = createRmCb(ch.elm.get, listeners)
            cbs.remove.foreach(hook => hook(ch, rm))
            ch.data.hook
              .flatMap(_.remove)
              .fold(rm()) { hook => hook(ch, rm); () }
          case None => // text node
            api.removeChild(parentElm, ch.elm.get)
        }
        i += 1
      }
    }

    def updateChildren(
        parentElm: dom.Node,
        oldCh: Array[VNode],
        newCh: Array[VNode],
        insertedVnodeQueue: VNodeQueue
    ): Unit = {

      assert(oldCh.nonEmpty)
      assert(newCh.nonEmpty)

      var oldStartIdx = 0
      var newStartIdx = 0
      var oldEndIdx = oldCh.length - 1
      var newEndIdx = newCh.length - 1

      var oldKeyToIdx: Map[String, Int] = null

      while (oldStartIdx <= oldEndIdx && newStartIdx <= newEndIdx) {
        if (oldCh(oldStartIdx) == null) {
          // Vnode might have been moved left
          oldStartIdx += 1
        } else if (oldCh(oldEndIdx) == null) {
          oldEndIdx -= 1
        } else if (sameVnode(oldCh(oldStartIdx), newCh(newStartIdx))) {
          newCh(newStartIdx) = patchVnode(
            oldCh(oldStartIdx),
            newCh(newStartIdx),
            insertedVnodeQueue
          )
          oldStartIdx += 1
          newStartIdx += 1
        } else if (sameVnode(oldCh(oldEndIdx), newCh(newEndIdx))) {
          newCh(newEndIdx) =
            patchVnode(oldCh(oldEndIdx), newCh(newEndIdx), insertedVnodeQueue)
          oldEndIdx -= 1
          newEndIdx -= 1
        } else if (sameVnode(oldCh(oldStartIdx), newCh(newEndIdx))) {
          // Vnode moved right
          newCh(newEndIdx) =
            patchVnode(oldCh(oldStartIdx), newCh(newEndIdx), insertedVnodeQueue)
          api.insertBefore(
            parentElm,
            oldCh(oldStartIdx).elm.get,
            api.nextSibling(oldCh(oldEndIdx).elm.get)
          )
          oldStartIdx += 1
          newEndIdx -= 1
        } else if (sameVnode(oldCh(oldEndIdx), newCh(newStartIdx))) {
          // Vnode moved left
          newCh(newStartIdx) =
            patchVnode(oldCh(oldEndIdx), newCh(newStartIdx), insertedVnodeQueue)
          api.insertBefore(
            parentElm,
            oldCh(oldEndIdx).elm.get,
            oldCh(oldStartIdx).elm
          )
          oldEndIdx -= 1
          newStartIdx += 1
        } else {
          if (oldKeyToIdx == null) {
            oldKeyToIdx = createKeyToOldIdx(oldCh, oldStartIdx, oldEndIdx)
          }
          val idxInOld = newCh(newStartIdx).key.flatMap { key =>
            oldKeyToIdx.get(key)
          }
          idxInOld match {
            case None =>
              // New element
              api.insertBefore(
                parentElm,
                createElm(newCh(newStartIdx), insertedVnodeQueue).elm.get,
                oldCh(oldStartIdx).elm
              )
            case Some(idxInOld) =>
              val elmToMove = oldCh(idxInOld)
              if (elmToMove.sel != newCh(newStartIdx).sel) {
                newCh(newStartIdx) =
                  createElm(newCh(newStartIdx), insertedVnodeQueue)
                api.insertBefore(
                  parentElm,
                  newCh(newStartIdx).elm.get,
                  oldCh(oldStartIdx).elm
                )
              } else {
                newCh(newStartIdx) =
                  patchVnode(elmToMove, newCh(newStartIdx), insertedVnodeQueue)
                oldCh(idxInOld) = null
                api.insertBefore(
                  parentElm,
                  elmToMove.elm.get,
                  oldCh(oldStartIdx).elm
                )
              }
          }
          newStartIdx += 1
        }
      }

      if (newStartIdx <= newEndIdx) {
        val before =
          if (newCh.length > newEndIdx + 1) newCh(newEndIdx + 1).elm
          else None
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
        vnode0: VNode,
        insertedVNodeQueue: VNodeQueue
    ): VNode = {
      val hook = vnode0.data.hook
      hook.flatMap(_.prepatch).foreach(hook => hook(oldVnode, vnode0))
      val elm = oldVnode.elm.get
      val oldCh = oldVnode.children

      if (oldVnode != vnode0) {

        val vnode = cbs.update.foldLeft(vnode0.copy(elm = Some(elm))) {
          case (vnode, hook) =>
            hook(oldVnode, vnode)
        }

        vnode.data.hook
          .flatMap(_.update)
          .foreach(hook => hook(oldVnode, vnode))

        val vnode1 = vnode.text match {
          case None =>
            (oldCh, vnode.children) match {
              case (Some(oldCh), Some(ch)) =>
                if (oldCh != ch) {
                  updateChildren(elm, oldCh, ch, insertedVNodeQueue)
                  vnode
                } else {
                  vnode
                }
              case (None, Some(ch)) =>
                oldVnode.text.foreach(_ => api.setTextContent(elm, Some("")))
                Some(
                  addVnodes(
                    elm,
                    None,
                    ch,
                    0,
                    ch.length - 1,
                    insertedVNodeQueue
                  )
                )
                vnode

              case (Some(oldCh), None) =>
                removeVnodes(elm, oldCh, 0, oldCh.length - 1)
                vnode
              case (None, None) =>
                oldVnode.text.foreach(_ => api.setTextContent(elm, Some("")))
                vnode
            }
          case Some(text) if oldVnode.text.forall(_ != text) =>
            oldCh.foreach(oldChildren =>
              removeVnodes(elm, oldChildren, 0, oldChildren.length - 1)
            )
            api.setTextContent(elm, Some(text))
            vnode
          case Some(_) => vnode
        }

        hook.flatMap(_.postpatch).foreach(hook => hook(oldVnode, vnode1))

        vnode1

      } else {

        oldVnode

      }

    }

    def patch(oldVnode: VNode, vnode: VNode): VNode = {

      val insertedVNodeQueue: VNodeQueue = mutable.ArrayBuffer.empty[VNode]
      cbs.pre.foreach(_())

      val vnode0 = if (sameVnode(oldVnode, vnode)) {
        patchVnode(oldVnode, vnode, insertedVNodeQueue)
      } else {
        val elm = oldVnode.elm.get
        val parent = api.parentNode(elm)
        val vnode1 = createElm(vnode, insertedVNodeQueue)
        parent match {
          case Some(parent) =>
            api.insertBefore(parent, vnode1.elm.get, api.nextSibling(elm))
            removeVnodes(parent, Array(oldVnode), 0, 0)
          case None => ()
        }
        vnode1
      }

      insertedVNodeQueue.foreach(vnode =>
        vnode.data.hook.flatMap(_.insert).foreach(_(vnode))
      )

      cbs.post.foreach(_())

      vnode0

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

  private def sameVnode(vnode1: VNode, vnode2: VNode): Boolean = {
    vnode1.key == vnode2.key &&
    vnode1.data.is == vnode2.data.is &&
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

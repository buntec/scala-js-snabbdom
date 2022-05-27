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

    type VNodeQueue = mutable.ArrayBuffer[PatchedVNode]

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

    def emptyNodeAt(elm: dom.Element): PatchedVNode = {
      val id = Option(elm.id).filter(_.nonEmpty).fold("")("#" + _)
      val classes = Option(elm.getAttribute("class"))
      val c = classes.map("." + _.split(" ").mkString(".")).getOrElse("")

      PatchedVNode(
        Some(api.tagName(elm).toLowerCase + id + c),
        VNodeData.empty,
        Nil,
        None,
        None,
        elm,
        None
      )

    }

    def emptyDocumentFragmentAt(frag: dom.DocumentFragment): PatchedVNode = {
      PatchedVNode(None, VNodeData.empty, Nil, None, None, frag, None)
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

    def createElm(
        vnode0: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode = {

      val vnode =
        vnode0.data.hook.flatMap(_.init).fold(vnode0)(hook => hook(vnode0))

      val sel = vnode.sel
      sel match {
        case Some("!") =>
          val text = vnode.text.getOrElse("")
          val elm = api.createComment(text)
          PatchedVNode(
            vnode.sel,
            vnode.data,
            Nil, // comment nodes can't have children
            Some(text),
            vnode.key,
            elm,
            None
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
          val elm = if (vnode.data.ns.isDefined) {
            api.createElementNS(
              vnode.data.ns.get,
              tag
            ) // TODO what about data?
          } else {
            api.createElement(tag) // TODO what about data argument?
          }
          val vnode0 = PatchedVNode(
            vnode.sel,
            vnode.data,
            children =
              vnode.children.map(ch => createElm(ch, insertedVNodeQueue)),
            text = vnode.text,
            key = vnode.key,
            elm = elm,
            None
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
            case List() =>
              vnode0.text match {
                case None => ()
                case Some(text) =>
                  api.appendChild(elm, api.createTextNode(text))
              }
            case children =>
              children.foreach { child =>
                api.appendChild(elm, child.elm)
              }
          }
          vnode0.data.hook.map { hooks =>
            hooks.create.foreach(hook => hook(vnode0))
            hooks.insert.foreach { _ => insertedVNodeQueue.append(vnode0) }
          }
          vnode0

        case None =>
          vnode.children match {
            case Nil =>
              PatchedVNode(
                vnode.sel,
                vnode.data,
                Nil,
                vnode.text,
                vnode.key,
                api.createTextNode(vnode.text.getOrElse("")),
                None
              )

            case children =>
              val elm = api.createDocumentFragment
              val vnode0 = PatchedVNode(
                vnode.sel,
                vnode.data,
                children =
                  children.map(ch => createElm(ch, insertedVNodeQueue)),
                text = vnode.text,
                key = vnode.key,
                elm = elm,
                None
              )
              cbs.create.foreach(hook => hook(vnode0))
              vnode0.children.foreach(ch => api.appendChild(elm, ch.elm))
              vnode0
          }

      }

    }

    def addAllVnodes(
        parentElm: dom.Node,
        before: Option[dom.Node],
        vnodes: List[VNode],
        insertedVNodeQueue: VNodeQueue
    ): List[PatchedVNode] = vnodes.map { vnode =>
      val pvnode = createElm(vnode, insertedVNodeQueue)
      api.insertBefore(
        parentElm,
        pvnode.elm,
        before
      )
      pvnode
    }

    def invokeDestroyHook(vnode: PatchedVNode): Unit = {
      if (!vnode.isTextNode) { // detroy hooks should not be called on text nodes
        vnode.data.hook.flatMap(_.destroy).foreach(hook => hook(vnode))
        cbs.destroy.foreach(hook => hook(vnode))
        vnode.children.foreach { child => invokeDestroyHook(child) }
      }
    }

    def removeAllVnodes(
        parentElm: dom.Node,
        vnodes: List[PatchedVNode]
    ): Unit = {
      vnodes.foreach { ch =>
        ch.sel match {
          case Some(_) =>
            invokeDestroyHook(ch)
            val listeners = cbs.remove.length + 1
            val rm = createRmCb(ch.elm, listeners)
            cbs.remove.foreach(hook => hook(ch, rm))
            ch.data.hook
              .flatMap(_.remove)
              .fold(rm()) { hook => hook(ch, rm); () }
          case None => // text node
            api.removeChild(parentElm, ch.elm)
        }

      }

    }

    def updateChildren(
        parentElm: dom.Node,
        oldCh: List[PatchedVNode],
        newCh: List[VNode],
        insertedVnodeQueue: VNodeQueue
    ): List[PatchedVNode] = {

      val (toDelete, patchedChildren) =
        newCh.foldLeft((oldCh, List.empty[PatchedVNode])) {
          case ((oh :: ot, acc), newCh) =>
            if (sameVnode(oh, newCh)) {
              val pn = patchVnode(oh, newCh, insertedVnodeQueue)
              (ot, pn :: acc)
            } else {
              val pn = createElm(newCh, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.elm, Some(oh.elm))
              (oh :: ot, pn :: acc)
            }
          case ((Nil, acc), newCh) =>
            val pn = createElm(newCh, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.elm, None)
            (Nil, pn :: acc)
        }

      removeAllVnodes(parentElm, toDelete)

      patchedChildren

    }

    def patchVnode(
        oldVnode: PatchedVNode,
        vnode00: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode = {
      val hook = vnode00.data.hook
      val vnode0 =
        hook.flatMap(_.prepatch).fold(vnode00)(hook => hook(oldVnode, vnode00))
      val elm = oldVnode.elm
      val oldCh = oldVnode.children

      if (oldVnode.toVNode != vnode0) {

        val vnode = cbs.update.foldLeft(vnode0) { case (vnode, hook) =>
          hook(oldVnode, vnode)
        }

        vnode.data.hook
          .flatMap(_.update)
          .foreach(hook => hook(oldVnode, vnode))

        val vnode1 = vnode.text match {
          case None =>
            (oldCh, vnode.children) match {
              case (oldCh @ _ :: _, ch @ _ :: _) =>
                if (oldCh != ch) {
                  PatchedVNode(
                    vnode.sel,
                    vnode.data,
                    updateChildren(elm, oldCh, ch, insertedVNodeQueue),
                    vnode.text,
                    vnode.key,
                    elm,
                    None
                  )

                } else {
                  PatchedVNode(
                    vnode.sel,
                    vnode.data,
                    oldCh,
                    vnode.text,
                    vnode.key,
                    elm,
                    None
                  )
                }
              case (Nil, ch @ _ :: _) =>
                oldVnode.text.foreach(_ => api.setTextContent(elm, Some("")))

                val patchedChildren = ch.map { vnode =>
                  val pvnode = createElm(vnode, insertedVNodeQueue)
                  api.insertBefore(
                    elm,
                    pvnode.elm,
                    None
                  )
                  pvnode
                }

                PatchedVNode(
                  vnode.sel,
                  vnode.data,
                  patchedChildren,
                  vnode.text,
                  vnode.key,
                  elm,
                  None
                )

              case (_ :: _, Nil) =>
                removeAllVnodes(elm, oldCh)
                PatchedVNode(
                  vnode.sel,
                  vnode.data,
                  Nil,
                  vnode.text,
                  vnode.key,
                  elm,
                  None
                )
              case (Nil, Nil) =>
                oldVnode.text.foreach(_ => api.setTextContent(elm, Some("")))
                PatchedVNode(
                  vnode.sel,
                  vnode.data,
                  Nil,
                  vnode.text,
                  vnode.key,
                  elm,
                  None
                )
            }
          case Some(text) if oldVnode.text.forall(_ != text) =>
            removeAllVnodes(elm, oldCh)
            api.setTextContent(elm, Some(text))
            PatchedVNode(
              vnode.sel,
              vnode.data,
              Nil,
              vnode.text,
              vnode.key,
              elm,
              None
            )
          case Some(_) =>
            PatchedVNode(
              vnode.sel,
              vnode.data,
              Nil,
              vnode.text,
              vnode.key,
              elm,
              None
            )
        }

        hook.flatMap(_.postpatch).foreach(hook => hook(oldVnode, vnode1))

        vnode1

      } else {

        oldVnode

      }

    }

    def patch(oldVnode: PatchedVNode, vnode: VNode): PatchedVNode = {

      val insertedVNodeQueue: VNodeQueue =
        mutable.ArrayBuffer.empty[PatchedVNode]
      cbs.pre.foreach(_())

      val vnode0 = if (sameVnode(oldVnode, vnode)) {
        patchVnode(oldVnode, vnode, insertedVNodeQueue)
      } else {
        val elm = oldVnode.elm
        val parent = api.parentNode(elm)
        val vnode1 = createElm(vnode, insertedVNodeQueue)
        parent match {
          case Some(parent) =>
            api.insertBefore(parent, vnode1.elm, api.nextSibling(elm))
            removeAllVnodes(parent, List(oldVnode))
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

      override def apply(oldVnode: PatchedVNode, vnode: VNode): PatchedVNode =
        patch(oldVnode, vnode)

      override def apply(elm: dom.Element, vnode: VNode): PatchedVNode =
        patch(emptyNodeAt(elm), vnode)

      override def apply(
          frag: dom.DocumentFragment,
          vnode: VNode
      ): PatchedVNode =
        patch(emptyDocumentFragmentAt(frag), vnode)

    }

  }

  private def sameVnode(vnode1: PatchedVNode, vnode2: VNode): Boolean = {
    vnode1.key == vnode2.key &&
    vnode1.data.is == vnode2.data.is &&
    vnode1.sel == vnode2.sel
  }

  private def createKeyToOldIdx(
      children: List[PatchedVNode]
  ): Map[String, Int] = {
    children.zipWithIndex
      .map { case (ch, i) =>
        ch.key.map { key => (key -> i) }
      }
      .collect { case Some(a) => a }
      .toMap
  }

}

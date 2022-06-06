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
import scala.annotation.tailrec

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
          postPatch =
            module.postPatch.fold(hooks.postPatch)(_ :: hooks.postPatch),
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

      PatchedVNode.Element(
        api.tagName(elm).toLowerCase + id + c,
        VNodeData.empty,
        Nil,
        elm
      )

    }

    def emptyDocumentFragmentAt(frag: dom.DocumentFragment): PatchedVNode = {
      PatchedVNode.Fragment(Nil, frag)
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

      vnode0 match {
        case VNode.Comment(content) =>
          val node = api.createComment(content)
          PatchedVNode.comment(content, node)

        case VNode.Fragment(children) =>
          val node = api.createDocumentFragment
          val patchedChildren =
            children.map(ch => createElm(ch, insertedVNodeQueue))
          val vnode = PatchedVNode.fragment(
            children = patchedChildren,
            node = node
          )
          cbs.create.foreach { hook => hook(vnode) }

          // insert children into dom
          patchedChildren.foreach(child => api.appendChild(node, child.node))

          vnode

        case VNode.Text(text) =>
          PatchedVNode.Text(
            text,
            api.createTextNode(text)
          )

        case VNode.Element(sel, data, children) =>
          data.hook.flatMap(_.init).foreach(hook => hook(vnode0))

          val hashIdx = sel.indexOf("#")
          val dotIdx = sel.indexOf(".", hashIdx)
          val hash = if (hashIdx > 0) hashIdx else sel.length
          val dot = if (dotIdx > 0) dotIdx else sel.length
          val tag = if (hashIdx != -1 || dotIdx != -1) {
            sel.slice(0, math.min(hash, dot))
          } else {
            sel
          }
          val elm = data.ns match {
            case Some(ns) =>
              api.createElementNS(ns, tag) // TODO what about data?
            case None =>
              api.createElement(tag) // TODO what about data argument?
          }
          val pvnode0 = PatchedVNode.Element(
            sel,
            data,
            children = children.map(createElm(_, insertedVNodeQueue)),
            node = elm
          )
          if (hash < dot) elm.setAttribute("id", sel.slice(hash + 1, dot))
          if (dotIdx > 0) {
            elm.setAttribute(
              "class",
              sel.slice(dot + 1, sel.length).replaceAll("""\.""", " ")
            )
          }

          cbs.create.foreach(hook => hook(pvnode0))

          // insert children into dom
          pvnode0.children.foreach(child => api.appendChild(elm, child.node))

          data.hook.flatMap(_.create).foreach(hook => hook(pvnode0))
          data.hook.flatMap(_.insert).foreach { _ =>
            insertedVNodeQueue.append(pvnode0)
          }

          pvnode0

      }

    }

    def invokeDestroyHook(vnode: PatchedVNode): Unit = {
      vnode match {
        case PatchedVNode.Element(_, data, children, _) =>
          data.hook.flatMap(_.destroy).foreach(_(vnode))
          cbs.destroy.foreach(_(vnode))
          children.foreach(invokeDestroyHook)
        case _ => ()
      }
    }

    def removeAllVnodes(
        parentElm: dom.Node,
        vnodes: List[PatchedVNode]
    ): Unit = {
      vnodes.foreach {
        case child @ PatchedVNode.Element(_, data, _, elm) =>
          invokeDestroyHook(child)
          val listeners = cbs.remove.length + 1
          val rm = createRmCb(elm, listeners)
          cbs.remove.foreach(_(child, rm))
          data.hook.flatMap(_.remove).fold(rm()) { hook =>
            hook(child, rm); ()
          }
        case PatchedVNode.Text(_, node) => // text node
          api.removeChild(parentElm, node)
        case _ => ()

      }

    }

    // This would be much simpler if we didn't have to
    // match up key'ed nodes even when their order and
    // position has completely changed.
    // TODO: Try to simplify and use better variable names.
    def updateChildren(
        parentElm: dom.Node,
        oldCh: List[PatchedVNode],
        newCh: List[VNode],
        insertedVnodeQueue: VNodeQueue
    ): List[PatchedVNode] = {

      // `toDelete1` - the old children in original order that were not
      // matched against new children.
      // `toDelete2` - the old children in reverse order that were not
      // matched against new children.
      // The intersection of toDelete1 and toDelete2 are nodes that
      // should either be deleted or are key'ed nodes that have been moved around.
      // `patchedChildrenWithIndex` - new children that are the result of
      // patching old children. Zipped with the index in the list of all
      // new children to allow merging with key'ed nodes later.
      // `newKeyed` - new children with keys that have not been matched
      // to old nodes. They are either new nodes or correspond to
      // old key'ed nodes that have been moved around.
      val (toDelete1, toDelete2, patchedChildrenWithIndex, newKeyed) =
        newCh.zipWithIndex.foldLeft(
          (
            oldCh,
            oldCh.reverse,
            List.empty[(PatchedVNode, Int)],
            List.empty[(VNode, Int)]
          )
        ) {
          case ((oh :: ot, oh2 :: ot2, acc, keyed), (newCh, i)) =>
            if (sameVnode(oh, newCh)) {
              val pn = patchVnode(oh, newCh, insertedVnodeQueue)
              if (oh == oh2) { // exhausted old child nodes
                (Nil, Nil, (pn, i) :: acc, keyed)
              } else {
                (ot, oh2 :: ot2, (pn, i) :: acc, keyed)
              }
            } else if (sameVnode(oh2, newCh)) {
              val pn = patchVnode(oh2, newCh, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.node, Some(oh.node))
              if (oh == oh2) { // exhausted old child nodes
                (Nil, Nil, (pn, i) :: acc, keyed)
              } else {
                (oh :: ot, ot2, (pn, i) :: acc, keyed)
              }
            } else if (VNode.key(newCh).isDefined) { // node with key - try to match later
              (oh :: ot, oh2 :: ot2, acc, (newCh, i) :: keyed)
            } else { // new node without key
              val pn = createElm(newCh, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.node, Some(oh.node))
              (oh :: ot, oh2 :: ot2, (pn, i) :: acc, keyed)
            }
          case (
                (Nil, _, acc, keyed),
                (newCh, i)
              ) => // old nodes are exhausted - must be new node
            val pn = createElm(newCh, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.node, None)
            (Nil, Nil, (pn, i) :: acc, keyed)
          case (
                (_, Nil, acc, keyed),
                (newCh, i)
              ) => // old nodes are exhausted - must be new node
            val pn = createElm(newCh, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.node, None)
            (Nil, Nil, (pn, i) :: acc, keyed)
        }

      // `toDelete` is the efficiently computed intersection of `toDelete1` and `toDelete2`.
      val (_, toDelete) =
        toDelete1.reverse.foldLeft((toDelete2, List.empty[PatchedVNode])) {
          case (((h :: t), acc), vnode) =>
            if (vnode == h) {
              (t, h :: acc)
            } else {
              (h :: t, acc)
            }
          case ((Nil, acc), _) => (Nil, acc)
        }

      // We split the old nodes that are candidates for deletion
      // into those with keys and those without. Those without keys
      // should certainly be deleted. Those with keys we'll try to match
      // up against new children with keys - any leftovers will be deleted
      val (toDeleteKeyed, toDeleteUnkeyed) =
        toDelete.partition(vnode => PatchedVNode.key(vnode).isDefined)

      val oldKeyed =
        toDeleteKeyed.map(n => (PatchedVNode.key(n).get -> n)).toMap

      // Here we match new, unmatched children with keys against
      // old children with keys. Any leftover old children with
      // keys will be returned for deletion.
      val (patchedKeyedChildrenWithIndex, toDeleteKeyed0) =
        newKeyed.foldLeft(
          (List.empty[(PatchedVNode, Int)], oldKeyed)
        ) { case ((acc1, acc2), (vnode, i)) =>
          acc2.get(VNode.key(vnode).get) match {
            case Some(pvnode) if sameVnode(pvnode, vnode) =>
              (
                (patchVnode(pvnode, vnode, insertedVnodeQueue), i) :: acc1,
                acc2 - VNode.key(vnode).get
              )
            case _ =>
              ((createElm(vnode, insertedVnodeQueue), i) :: acc1, acc2)
          }
        }

      removeAllVnodes(parentElm, toDeleteUnkeyed)
      removeAllVnodes(parentElm, toDeleteKeyed0.values.toList)

      // A recursive helper function to merge child nodes that were patched or created
      // in the first fold, and those new key'ed child nodes that were
      // matched up with old key'ed children. The result is the
      // list of all patched children in reverse order.
      @tailrec def merge(
          v1: List[(PatchedVNode, Int)],
          v2: List[(PatchedVNode, Int)],
          acc: List[PatchedVNode]
      ): List[PatchedVNode] = {
        (v1, v2) match {
          case (((h1, i) :: t1), ((h2, j) :: t2)) =>
            if (j < i) {
              api.insertBefore(parentElm, h2.node, Some(h1.node))
              merge(v1, t2, h2 :: acc)
            } else {
              assert(j > i) // v1 and v2 are disjoint!
              merge(t1, v2, h1 :: acc)
            }
          case (Nil, ((h2, _) :: t2)) =>
            api.insertBefore(parentElm, h2.node, None)
            merge(Nil, t2, h2 :: acc)
          case (((h1, _) :: t1), Nil) => merge(t1, Nil, h1 :: acc)
          case (Nil, Nil)             => acc
        }
      }

      merge(
        patchedChildrenWithIndex.reverse,
        patchedKeyedChildrenWithIndex,
        List.empty
      ).reverse

    }

    def patchVnode(
        oldVnode: PatchedVNode,
        vnode: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode = {

      // apply prepatch hooks
      vnode match {
        case VNode.Element(_, data, _) =>
          data.hook.flatMap(_.prepatch).foreach(hook => hook(oldVnode, vnode))
        case _ => ()
      }

      if (vnode == oldVnode.toVNode) {

        oldVnode // nothing to do

      } else {

        // apply update hooks
        cbs.update.foreach { hook => hook(oldVnode, vnode) }
        vnode match {
          case VNode.Element(_, data, _) =>
            data.hook.flatMap(_.update).foreach(hook => hook(oldVnode, vnode))
          case _ => ()
        }

        val patchedVNode = (oldVnode, vnode) match {

          case (
                PatchedVNode.Fragment(oldCh, elm),
                VNode.Fragment(ch)
              ) =>
            (oldCh, ch) match {
              case (oldCh @ _ :: _, ch @ _ :: _) =>
                if (oldCh.map(_.toVNode) != ch) {
                  PatchedVNode.fragment(
                    updateChildren(elm, oldCh, ch, insertedVNodeQueue),
                    elm
                  )
                } else {
                  PatchedVNode.fragment(
                    oldCh,
                    elm
                  )
                }

              case (Nil, ch @ _ :: _) =>
                val patchedChildren = ch.map { vnode =>
                  val pvnode = createElm(vnode, insertedVNodeQueue)
                  api.insertBefore(
                    elm,
                    pvnode.node,
                    None
                  )
                  pvnode
                }
                PatchedVNode.fragment(
                  patchedChildren,
                  elm
                )

              case (oldCh @ _ :: _, Nil) =>
                removeAllVnodes(elm, oldCh)
                PatchedVNode.fragment(
                  Nil,
                  elm
                )
              case (Nil, Nil) =>
                PatchedVNode.fragment(
                  Nil,
                  elm
                )

            }

          case (
                PatchedVNode.Comment(oldContent, node),
                VNode.Comment(content)
              ) =>
            if (oldContent != content) {
              api.setTextContent(node, Some(content))
            }
            PatchedVNode.Comment(content, node)

          case (PatchedVNode.Text(oldContent, node), VNode.Text(content)) =>
            if (oldContent != content) {
              api.setTextContent(node, Some(content))
            }
            PatchedVNode.Text(
              content,
              node
            )

          case (
                PatchedVNode.Element(_, _, oldCh, elm),
                VNode.Element(sel, data, ch)
              ) =>
            (oldCh, ch) match {
              case (oldCh @ _ :: _, ch @ _ :: _) =>
                if (oldCh.map(_.toVNode) != ch) {
                  PatchedVNode.element(
                    sel,
                    data,
                    updateChildren(elm, oldCh, ch, insertedVNodeQueue),
                    elm
                  )
                } else {
                  PatchedVNode.element(
                    sel,
                    data,
                    oldCh,
                    elm
                  )
                }

              case (Nil, ch @ _ :: _) =>
                val patchedChildren = ch.map { vnode =>
                  val pvnode = createElm(vnode, insertedVNodeQueue)
                  api.insertBefore(
                    elm,
                    pvnode.node,
                    None
                  )
                  pvnode
                }
                PatchedVNode.Element(
                  sel,
                  data,
                  patchedChildren,
                  elm
                )

              case (oldCh @ _ :: _, Nil) =>
                removeAllVnodes(elm, oldCh)
                PatchedVNode.Element(
                  sel,
                  data,
                  Nil,
                  elm
                )
              case (Nil, Nil) =>
                PatchedVNode.Element(
                  sel,
                  data,
                  Nil,
                  elm
                )

            }

          case _ =>
            throw new IllegalStateException(
              "trying to patch incompatible nodes"
            )

        }

        // apply post patch hooks
        cbs.postPatch.foreach(hook => hook(oldVnode, patchedVNode))
        patchedVNode match {
          case PatchedVNode.Element(_, data, _, _) =>
            data.hook
              .flatMap(_.postpatch)
              .foreach(hook => hook(oldVnode, patchedVNode))
          case _ => ()
        }

        patchedVNode

      }

    }

    def patch(oldVnode: PatchedVNode, vnode: VNode): PatchedVNode = {

      val insertedVNodeQueue: VNodeQueue =
        mutable.ArrayBuffer.empty[PatchedVNode]

      cbs.pre.foreach(_())

      val vnode0 = if (sameVnode(oldVnode, vnode)) {
        patchVnode(oldVnode, vnode, insertedVNodeQueue)
      } else {
        val elm = oldVnode.node
        val parent = api.parentNode(elm)
        val vnode1 = createElm(vnode, insertedVNodeQueue)
        parent match {
          case Some(parent) =>
            api.insertBefore(parent, vnode1.node, api.nextSibling(elm))
            removeAllVnodes(parent, List(oldVnode))
          case None => ()
        }
        vnode1
      }

      insertedVNodeQueue.foreach {
        case vnode @ PatchedVNode.Element(_, data, _, _) =>
          data.hook.flatMap(_.insert).foreach(_(vnode))
        case _ => ()
      }

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
    (vnode1.toVNode, vnode2) match {
      case (VNode.Text(_), VNode.Text(_))         => true
      case (VNode.Fragment(_), VNode.Fragment(_)) => true
      case (VNode.Element(sel1, data1, _), VNode.Element(sel2, data2, _)) =>
        data1.key == data2.key && data1.is == data2.is && sel1 == sel2
      case _ => false
    }
  }

}

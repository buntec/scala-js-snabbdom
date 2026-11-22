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
import snabbdom.modules._

import scala.annotation.tailrec
import scala.collection.mutable

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

    def emptyNodeAt(elm: dom.Element, vnode: VNode): PatchedVNode = {
      val (sel, elmNs) = toVNode.elementIdentity(elm, api)
      // A root vnode without an explicit namespace adopts the container's
      // namespace, so mounting `h("g", ...)` onto an existing SVG <g> patches
      // it in place instead of replacing it with an HTML-namespace element.
      val ns = vnode match {
        case VNode.Element(_, data, _) if data.ns.isEmpty => None
        case _                                            => elmNs
      }
      PatchedVNode.Element(
        sel,
        if (ns.isEmpty) VNodeData.empty else VNodeData(ns = ns),
        Nil,
        elm
      )

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
        vnode: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode =
      createInitializedElm(VNode.applyInitHook(vnode), insertedVNodeQueue)

    def createInitializedElm(
        vnode: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode = {
      vnode match {
        case VNode.Comment(content) =>
          val node = api.createComment(content)
          PatchedVNode.comment(content, node)

        case VNode.Text(text) =>
          PatchedVNode.Text(
            text,
            api.createTextNode(text)
          )

        case VNode.Element(sel, data, children) =>
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

    def replaceVnode(
        oldVnode: PatchedVNode
    )(create: => PatchedVNode): PatchedVNode = {
      val elm = oldVnode.node
      val parent = api.parentNode(elm)
      val replacement = create
      parent.foreach { parent =>
        api.insertBefore(parent, replacement.node, api.nextSibling(elm))
        removeAllVnodes(parent, List(oldVnode))
      }
      replacement
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
        case child =>
          api.removeChild(parentElm, child.node)

      }

    }

    def updateChildren(
        parentElm: dom.Node,
        oldCh: List[PatchedVNode],
        newCh: List[VNode],
        insertedVNodeQueue: VNodeQueue
    ): List[PatchedVNode] = {
      val patched = List.newBuilder[PatchedVNode]
      var oldRest = oldCh
      var newRest = newCh

      // Walk the common prefix without building indexes or reversing siblings.
      while (
        oldRest.nonEmpty && newRest.nonEmpty && sameVnode(
          oldRest.head,
          newRest.head
        )
      ) {
        patched += patchVnode(oldRest.head, newRest.head, insertedVNodeQueue)
        oldRest = oldRest.tail
        newRest = newRest.tail
      }

      if (newRest.isEmpty) {
        removeAllVnodes(parentElm, oldRest)
      } else if (oldRest.isEmpty) {
        newRest.foreach { vnode =>
          val child = createElm(vnode, insertedVNodeQueue)
          api.insertBefore(parentElm, child.node, None)
          patched += child
        }
      } else {
        patched ++= reconcileChildren(
          parentElm,
          oldRest,
          newRest,
          insertedVNodeQueue
        )
      }

      patched.result()
    }

    // This would be much simpler if we didn't have to
    // match up key'ed nodes even when their order and
    // position has completely changed.
    // TODO: Try to simplify and use better variable names.
    def reconcileChildren(
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
              if (oh.node eq oh2.node) { // exhausted old child nodes
                (Nil, Nil, (pn, i) :: acc, keyed)
              } else {
                (ot, oh2 :: ot2, (pn, i) :: acc, keyed)
              }
            } else if (sameVnode(oh2, newCh)) {
              val pn = patchVnode(oh2, newCh, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.node, Some(oh.node))
              if (oh.node eq oh2.node) { // exhausted old child nodes
                (Nil, Nil, (pn, i) :: acc, keyed)
              } else {
                (oh :: ot, ot2, (pn, i) :: acc, keyed)
              }
            } else if (newCh.key.isDefined) { // node with key - try to match later
              (oh :: ot, oh2 :: ot2, acc, (newCh, i) :: keyed)
            } else { // new node without key
              val pn = createElm(newCh, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.node, Some(oh.node))
              (oh :: ot, oh2 :: ot2, (pn, i) :: acc, keyed)
            }
          case ((Nil, _, acc, keyed), (newCh, i)) =>
            // old nodes are exhausted - must be new node
            val pn = createElm(newCh, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.node, None)
            (Nil, Nil, (pn, i) :: acc, keyed)
          case ((_, Nil, acc, keyed), (newCh, i)) =>
            // old nodes are exhausted - must be new node
            val pn = createElm(newCh, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.node, None)
            (Nil, Nil, (pn, i) :: acc, keyed)
        }

      // `toDelete` is the efficiently computed intersection of `toDelete1` and `toDelete2`.
      val (_, toDelete) =
        toDelete1.reverse.foldLeft((toDelete2, List.empty[PatchedVNode])) {
          case (((h :: t), acc), vnode) =>
            if (vnode.node eq h.node) {
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
      val toDeleteUnmatched = List.newBuilder[PatchedVNode]
      val oldKeyed = mutable.LinkedHashMap.empty[String, PatchedVNode]
      toDelete.foreach { vnode =>
        PatchedVNode.key(vnode) match {
          case Some(key) =>
            // Older siblings with the same key still need to be removed.
            oldKeyed.put(key, vnode).foreach(toDeleteUnmatched += _)
          case None => toDeleteUnmatched += vnode
        }
      }

      // Here we match new, unmatched children with keys against
      // old children with keys. Any leftover old children with
      // keys will be returned for deletion.
      val patchedKeyedChildrenWithIndex =
        newKeyed.foldLeft(List.empty[(PatchedVNode, Int)]) {
          case (acc, (vnode, i)) =>
            val key = vnode.key.get
            oldKeyed.get(key) match {
              case Some(pvnode) if sameVnode(pvnode, vnode) =>
                oldKeyed.remove(key)
                (patchVnode(pvnode, vnode, insertedVnodeQueue), i) :: acc
              case _ =>
                (createElm(vnode, insertedVnodeQueue), i) :: acc
            }
        }

      removeAllVnodes(parentElm, toDeleteUnmatched.result())
      removeAllVnodes(parentElm, oldKeyed.values.toList)

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
              api.insertBefore(
                parentElm,
                h2.node,
                Some(h1.node)
              )
              merge(v1, t2, h2 :: acc)
            } else {
              assert(j > i) // v1 and v2 are disjoint!
              merge(t1, v2, h1 :: acc)
            }
          case (Nil, ((h2, _) :: t2)) =>
            api.insertBefore(parentElm, h2.node, None)
            merge(Nil, t2, h2 :: acc)
          case (((h1, _) :: t1), Nil) =>
            merge(t1, Nil, h1 :: acc)
          case (Nil, Nil) =>
            acc
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
        vnode00: VNode,
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode = {

      // A thunk whose fn and args are unchanged keeps its rendered subtree, so
      // skip rebuilding and diffing it.
      (oldVnode, vnode00) match {
        case (old: PatchedVNode.Element, VNode.Element(_, data, _))
            if data.fn.isDefined && data.fn == old.data.fn &&
              data.args == old.data.args =>
          return oldVnode
        case _ => ()
      }

      // apply prepatch hooks
      val vnode = vnode00 match {
        case VNode.Element(_, data, _) =>
          data.hook
            .flatMap(_.prepatch)
            .fold(vnode00)(hook => hook(oldVnode, vnode00))
        case _ => vnode00
      }

      val namespaceChanged = (vnode ne vnode00) && ((oldVnode, vnode) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          old.data.ns != next.data.ns
        case _ => false
      })
      if (namespaceChanged) {
        return replaceVnode(oldVnode)(
          createInitializedElm(vnode, insertedVNodeQueue)
        )
      }

      val childrenUnchanged = (oldVnode, vnode) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          sameChildren(old.children, next.children)
        case _ => false
      }
      val unchanged = (oldVnode, vnode) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          childrenUnchanged && old.sel == next.sel && old.data == next.data
        case (PatchedVNode.Text(old, _), VNode.Text(next))       => old == next
        case (PatchedVNode.Comment(old, _), VNode.Comment(next)) => old == next
        case _                                                   => false
      }

      if (unchanged) {

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
                PatchedVNode.Comment(_, node),
                VNode.Comment(content)
              ) =>
            api.setTextContent(node, Some(content))
            PatchedVNode.Comment(content, node)

          case (PatchedVNode.Text(_, node), VNode.Text(content)) =>
            api.setTextContent(node, Some(content))
            PatchedVNode.Text(
              content,
              node
            )

          case (
                PatchedVNode.Element(_, _, oldCh, elm),
                VNode.Element(sel, data, ch)
              ) =>
            val children =
              if (childrenUnchanged) oldCh
              else updateChildren(elm, oldCh, ch, insertedVNodeQueue)
            PatchedVNode.Element(sel, data, children, elm)

          case _ =>
            throw new AssertionError("trying to patch incompatible nodes")

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
        replaceVnode(oldVnode)(createElm(vnode, insertedVNodeQueue))
      }

      insertedVNodeQueue.foreach {
        case vnode @ PatchedVNode.Element(_, data, _, _) =>
          data.hook.flatMap(_.insert).foreach(_(vnode))
        case _ => ()
      }

      cbs.post.foreach(_())

      vnode0

    }

    Patch(
      (oldVnode, vnode) => patch(oldVnode, vnode),
      (elm, vnode) => patch(emptyNodeAt(elm, vnode), vnode)
    )

  }

  private def sameVnode(vnode1: PatchedVNode, vnode2: VNode): Boolean = {
    (vnode1, vnode2) match {
      case (_: PatchedVNode.Text, _: VNode.Text)            => true
      case (_: PatchedVNode.Comment, _: VNode.Comment)      => true
      case (old: PatchedVNode.Element, next: VNode.Element) =>
        old.sel == next.sel && old.data.key == next.data.key &&
        old.data.is == next.data.is &&
        // A thunk can supply its namespace when prepatch resolves it.
        (old.data.ns == next.data.ns || (next.data.ns.isEmpty && next.data.fn.nonEmpty))
      case _ => false
    }
  }

  @tailrec private def sameChildren(
      old: List[PatchedVNode],
      next: List[VNode]
  ): Boolean = {
    (old, next) match {
      case (Nil, Nil)                                 => true
      case (oldHead :: oldTail, nextHead :: nextTail) =>
        sameTree(oldHead, nextHead) && sameChildren(oldTail, nextTail)
      case _ => false
    }
  }

  private def sameTree(old: PatchedVNode, next: VNode): Boolean = {
    (old, next) match {
      case (old: PatchedVNode.Element, next: VNode.Element) =>
        old.sel == next.sel && old.data == next.data && sameChildren(
          old.children,
          next.children
        )
      case (PatchedVNode.Text(old, _), VNode.Text(next))       => old == next
      case (PatchedVNode.Comment(old, _), VNode.Comment(next)) => old == next
      case _                                                   => false
    }
  }

}

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
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

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

    // Old elements whose children a `sameTree` walk, while patching an
    // ancestor, found to differ from those of the paired new vnode. Lets
    // `patchVnode` skip walking them again, which would otherwise make a
    // change deep in the tree cost quadratic time in its depth. Allocated
    // lazily and scoped to a single `patch` call.
    var childrenDiffer: IdentityMap[PatchedVNode, VNode] = null

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
      val (sel, ns) = toVNode.elementIdentity(elm, api)
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
        ctx: Option[String],
        insertedVNodeQueue: VNodeQueue
    ): PatchedVNode =
      createInitializedElm(VNode.applyInitHook(vnode), ctx, insertedVNodeQueue)

    def createInitializedElm(
        vnode: VNode,
        ctx: Option[String],
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
          val elm = data.ns.orElse(ctx) match {
            case Some(ns) => api.createElementNS(ns, tag)
            case None     => api.createElement(tag)
          }
          val childCtx = childContext(elm)
          val pvnode0 = PatchedVNode.Element(
            sel,
            data,
            children = children.map(createElm(_, childCtx, insertedVNodeQueue)),
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

    // The namespace a vnode without `ns` created in place of `oldVnode`
    // inherits from the DOM parent.
    def parentContext(oldVnode: PatchedVNode): Option[String] =
      api.parentNode(oldVnode.node) match {
        case Some(parent: dom.Element) => childContext(parent)
        case _                         => None
      }

    def knownChildrenDiffer(
        old: PatchedVNode.Element,
        next: VNode.Element
    ): Boolean = {
      val marks = childrenDiffer
      marks != null && marks.get(old).exists(_ eq next)
    }

    @tailrec def sameChildren(
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

    def sameTree(old: PatchedVNode, next: VNode): Boolean = {
      (old, next) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          old.sel == next.sel && old.data == next.data && {
            val same = sameChildren(old.children, next.children)
            if (!same) {
              if (childrenDiffer == null) childrenDiffer = new IdentityMap()
              childrenDiffer.set(old, next)
            }
            same
          }
        case (PatchedVNode.Text(old, _), VNode.Text(next))       => old == next
        case (PatchedVNode.Comment(old, _), VNode.Comment(next)) => old == next
        case _                                                   => false
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
        case child =>
          // The node may already be gone, e.g. replaced via `innerHTML`.
          if (api.parentNode(child.node).exists(_ eq parentElm)) {
            api.removeChild(parentElm, child.node)
          }

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
      val ctx = parentElm match {
        case elm: dom.Element => childContext(elm)
        case _                => None
      }

      // Walk the common prefix without building indexes or reversing siblings.
      while (
        oldRest.nonEmpty && newRest.nonEmpty &&
        sameVnode(oldRest.head, newRest.head)
      ) {
        patched += patchVnode(oldRest.head, newRest.head, insertedVNodeQueue)
        oldRest = oldRest.tail
        newRest = newRest.tail
      }

      if (newRest.isEmpty) {
        removeAllVnodes(parentElm, oldRest)
      } else if (oldRest.isEmpty) {
        newRest.foreach { vnode =>
          val child = createElm(vnode, ctx, insertedVNodeQueue)
          api.insertBefore(parentElm, child.node, None)
          patched += child
        }
      } else {
        patched ++= reconcileChildren(
          parentElm,
          oldRest,
          newRest,
          ctx,
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
        ctx: Option[String],
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
              val pn = createElm(newCh, ctx, insertedVnodeQueue)
              api.insertBefore(parentElm, pn.node, Some(oh.node))
              (oh :: ot, oh2 :: ot2, (pn, i) :: acc, keyed)
            }
          case ((Nil, _, acc, keyed), (newCh, i)) =>
            // old nodes are exhausted - must be new node
            val pn = createElm(newCh, ctx, insertedVnodeQueue)
            api.insertBefore(parentElm, pn.node, None)
            (Nil, Nil, (pn, i) :: acc, keyed)
          case ((_, Nil, acc, keyed), (newCh, i)) =>
            // old nodes are exhausted - must be new node
            val pn = createElm(newCh, ctx, insertedVnodeQueue)
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

      // Old candidates with keys may still be matched against new children
      // with keys. Of several old siblings sharing a key only the last one is
      // a candidate. Every candidate that ends up unmatched is deleted.
      val oldKeyed = mutable.HashMap.empty[String, PatchedVNode]
      toDelete.foreach { vnode =>
        PatchedVNode.key(vnode).foreach(oldKeyed.put(_, vnode))
      }
      val matched = new IdentityMap[PatchedVNode, PatchedVNode]()

      // Here we match new, unmatched children with keys against
      // old children with keys.
      val patchedKeyedChildrenWithIndex =
        newKeyed.foldLeft(List.empty[(PatchedVNode, Int)]) {
          case (acc, (vnode, i)) =>
            val key = vnode.key.get
            oldKeyed.get(key) match {
              case Some(pvnode) if sameVnode(pvnode, vnode) =>
                oldKeyed.remove(key)
                matched.set(pvnode, pvnode)
                (patchVnode(pvnode, vnode, insertedVnodeQueue), i) :: acc
              case _ =>
                (createElm(vnode, ctx, insertedVnodeQueue), i) :: acc
            }
        }

      // `toDelete` is in DOM order, so destroy and remove hooks are too.
      removeAllVnodes(parentElm, toDelete.filterNot(matched.has))

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

      // A library thunk whose fn and args are unchanged would re-render to the
      // same subtree, so skip rebuilding and diffing it. Thunks whose prepatch
      // hook was replaced by the caller take the regular path.
      (oldVnode, vnode00) match {
        case (old: PatchedVNode.Element, VNode.Element(_, data, _))
            if data.fn.isDefined && data.fn == old.data.fn &&
              data.args == old.data.args && isLibraryThunk(data) =>
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

      // The element is replaced if the vnode, as returned by the prepatch
      // hook, names a namespace the element is not in (see `inNamespace`).
      val namespaceChanged = (oldVnode, vnode) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          next.data.ns.exists(!inNamespace(old.node, _))
        case _ => false
      }
      if (namespaceChanged) {
        val ctx = parentContext(oldVnode)
        // A library thunk's prepatch hook has already rendered it; any other
        // vnode is initialised like a newly created one.
        val replacement = vnode00 match {
          case VNode.Element(_, data, _) if isLibraryThunk(data) =>
            createInitializedElm(vnode, ctx, insertedVNodeQueue)
          case _ => createElm(vnode, ctx, insertedVNodeQueue)
        }
        return replaceVnode(oldVnode)(replacement)
      }

      val childrenUnchanged = (oldVnode, vnode) match {
        case (old: PatchedVNode.Element, next: VNode.Element) =>
          !((vnode eq vnode00) && knownChildrenDiffer(old, next)) &&
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
      val saved = childrenDiffer
      childrenDiffer = null
      try patch0(oldVnode, vnode)
      finally childrenDiffer = saved
    }

    def patch0(oldVnode: PatchedVNode, vnode: VNode): PatchedVNode = {

      val insertedVNodeQueue: VNodeQueue =
        mutable.ArrayBuffer.empty[PatchedVNode]

      cbs.pre.foreach(_())

      val vnode0 = if (sameVnode(oldVnode, vnode)) {
        patchVnode(oldVnode, vnode, insertedVNodeQueue)
      } else {
        replaceVnode(oldVnode)(
          createElm(vnode, parentContext(oldVnode), insertedVNodeQueue)
        )
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
      (elm, vnode) => patch(emptyNodeAt(elm), vnode)
    )

  }

  // An ES2015 Map: keys are compared by identity, not by structural equality.
  @js.native
  @JSGlobal("Map")
  private class IdentityMap[K, V <: AnyRef] extends js.Object {
    def get(key: K): js.UndefOr[V] = js.native
    def set(key: K, value: V): this.type = js.native
    def has(key: K): Boolean = js.native
  }

  // Namespaces
  //
  // Creating an element: a vnode with `ns` is created in that namespace. A
  // vnode without `ns` inherits the namespace of the DOM element it is created
  // in: SVG inside SVG except inside `foreignObject`, and otherwise whatever
  // `DomApi.createElement` produces (HTML, or no namespace in an XML
  // document). `h("svg", ...)` names the SVG namespace on its descendants.
  //
  // Reusing an element: namespaces play no part in matching old and new
  // vnodes (`sameVnode`). Once the prepatch hook has run, a matched element is
  // replaced only if the vnode names a namespace the element is not in. A
  // vnode without `ns` keeps the element in whatever namespace it is, so
  // moving an element to HTML takes an explicit HTML `ns`.

  private val htmlNamespace = "http://www.w3.org/1999/xhtml"

  // Whether `elm` is in namespace `ns`, where `""` stands for no namespace.
  private def inNamespace(elm: dom.Element, ns: String): Boolean =
    if (ns.isEmpty) elm.namespaceURI == null else elm.namespaceURI == ns

  // The namespace children without `ns` inherit from `parent`: `None` leaves
  // it to `DomApi.createElement`, `Some("")` is no namespace.
  private def childContext(parent: dom.Element): Option[String] =
    parent.namespaceURI match {
      case `htmlNamespace`                                       => None
      case null                                                  => Some("")
      case h.svgNamespace if parent.localName == "foreignObject" => None
      case ns                                                    => Some(ns)
    }

  private def isLibraryThunk(data: VNodeData): Boolean =
    data.hook.exists(_.prepatch.exists(_ eq thunk.prepatchHook))

  private def sameVnode(vnode1: PatchedVNode, vnode2: VNode): Boolean = {
    (vnode1, vnode2) match {
      case (_: PatchedVNode.Text, _: VNode.Text)            => true
      case (_: PatchedVNode.Comment, _: VNode.Comment)      => true
      case (old: PatchedVNode.Element, next: VNode.Element) =>
        old.sel == next.sel && old.data.key == next.data.key &&
        old.data.is == next.data.is
      case _ => false
    }
  }

}

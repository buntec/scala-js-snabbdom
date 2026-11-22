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

package snabbdom.modules

import org.scalajs.dom
import snabbdom._

import scala.scalajs.js

object EventListeners {

  // The listener lives on the DOM node so that every `PatchedVNode` for that
  // node (including stale or copied ones) resolves to the same callback.
  private val ListenerProperty = "__snabbdomListener"

  private def listenerOf(node: dom.Node): PatchedVNode.Listener = {
    val l = node.asInstanceOf[js.Dynamic].selectDynamic(ListenerProperty)
    if (js.isUndefined(l)) null else l.asInstanceOf[PatchedVNode.Listener]
  }

  private def setListener(
      node: dom.Node,
      listener: PatchedVNode.Listener
  ): Unit =
    node
      .asInstanceOf[js.Dynamic]
      .updateDynamic(ListenerProperty)(listener.asInstanceOf[js.Any])

  private def clearListener(node: dom.Node): Unit =
    js.special.delete(node.asInstanceOf[js.Dynamic], ListenerProperty)

  val module: Module = Module().copy(
    create = Some((vNode: PatchedVNode) => {
      vNode match {
        case elm: PatchedVNode.Element if elm.data.on.nonEmpty =>
          sync(elm)
        case _ => ()
      }
    }),
    postPatch = Some((_: PatchedVNode, vNode: PatchedVNode) =>
      vNode match {
        case elm: PatchedVNode.Element => sync(elm)
        case _                         => ()
      }
    ),
    destroy = Some((vnode: PatchedVNode) => {
      vnode match {
        case elm: PatchedVNode.Element =>
          val listener = listenerOf(elm.node)
          if (listener != null) {
            listener.on.keysIterator.foreach { name =>
              elm.node.removeEventListener(name, listener.callback, false)
            }
            clearListener(elm.node)
          }
        case _ => ()
      }
    })
  )

  // Brings the node's registered events in line with `vnode.data.on`,
  // diffing against what is actually registered rather than an old vnode.
  private def sync(vnode: PatchedVNode.Element): Unit = {
    val elm = vnode.node
    val on = vnode.data.on
    val existing = listenerOf(elm)

    if (existing == null) {
      if (on.nonEmpty) {
        val listener = new PatchedVNode.Listener(vnode)
        listener.on = on
        setListener(elm, listener)
        on.keysIterator.foreach { name =>
          elm.addEventListener(name, listener.callback, false)
        }
      }
    } else {
      existing.vnode = vnode
      val registered = existing.on
      // The callback stays registered for events present in both versions.
      if (registered ne on) {
        registered.keysIterator.filterNot(on.contains).foreach { name =>
          elm.removeEventListener(name, existing.callback, false)
        }
        on.keysIterator.filterNot(registered.contains).foreach { name =>
          elm.addEventListener(name, existing.callback, false)
        }
        existing.on = on
      }
      if (on.isEmpty) clearListener(elm)
    }
  }

}

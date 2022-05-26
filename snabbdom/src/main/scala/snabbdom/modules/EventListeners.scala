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

import snabbdom._
import org.scalajs.dom

object EventListeners {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(vNode: VNode): Unit = {
        updateEventListeners(None, Some(vNode))
        ()
      }
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): VNode =
        updateEventListeners(Some(oldVNode), Some(vNode)).get
    }),
    destroy = Some(new DestroyHook {
      override def apply(vnode: VNode): Unit = {
        updateEventListeners(Some(vnode), None)
        ()
      }
    })
  )

  private def createListener(vnode: VNode) = {
    new Listener(vnode)
  }

  private def updateEventListeners(
      oldVnode: Option[VNode],
      vnode: Option[VNode]
  ): Option[VNode] = {

    val oldOn = oldVnode.map(_.data.on).getOrElse(Map.empty)
    val oldListener = oldVnode.flatMap(_.listener)
    val oldElm = oldVnode.flatMap(_.elm).map(_.asInstanceOf[dom.Element])
    val on = vnode.map(_.data.on).getOrElse(Map.empty)
    val elm = vnode.flatMap(_.elm).map(_.asInstanceOf[dom.Element])

    if (oldOn != on) {

      if (oldOn.nonEmpty && oldListener.isDefined) {
        val ol = oldListener.get
        if (on.isEmpty) {
          oldOn.foreach { case (name, _) =>
            oldElm.foreach { elm =>
              elm.removeEventListener(name, ol.jsFun, false)
            }
          }
        } else {
          oldOn.foreach { case (name, _) =>
            if (on.get(name).isEmpty) {
              oldElm.foreach(
                _.removeEventListener(name, ol.jsFun, false)
              )
            }
          }
        }
      }

      if (on.nonEmpty) {

        val vnode0 = vnode.get

        val listener = oldListener.getOrElse(createListener(vnode0))
        listener.vnode = vnode0

        if (oldOn.isEmpty) {
          on.foreach { case (name, _) =>
            elm.foreach(_.addEventListener(name, listener.jsFun, false))
          }
        } else {
          on.foreach { case (name, _) =>
            if (!oldOn.contains(name)) {
              elm.foreach(_.addEventListener(name, listener.jsFun, false))
            }
          }
        }

        Some(vnode0.copy(listener = Some(listener)))

      } else {
        None
      }

    } else {
      vnode
    }

  }

}

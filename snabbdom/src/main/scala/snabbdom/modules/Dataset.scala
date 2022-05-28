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
import scalajs.js

object Dataset {

  val module: Module = Module().copy(
    create = Some((vNode: PatchedVNode) => {
      if (vNode.data.dataset.nonEmpty) {
        setDataset(vNode)
      }
      vNode
    }),
    update = Some((oldVNode: PatchedVNode, vNode: VNode) => {
      if (vNode.data.dataset != oldVNode.data.dataset) {
        updateDataset(oldVNode, vNode)
      }
      vNode
    })
  )

  private val CAPS_REGEX = "[A-Z]"

  private def setDataset(vnode: PatchedVNode): Unit = {

    val elm = vnode.elm.asInstanceOf[dom.HTMLElement]
    val d = elm.dataset
    val dataset = vnode.data.dataset

    dataset.foreach { case (key, value) =>
      if (!js.isUndefined(d)) { // TODO: does this make sense?
        d += (key -> value)
      } else {
        elm.setAttribute(
          "data-" + key.replaceAll(CAPS_REGEX, "-$&").toLowerCase(),
          value
        )
      }

    }

  }

  private def updateDataset(oldVnode: PatchedVNode, vnode: VNode): Unit = {

    val elm = oldVnode.elm.asInstanceOf[dom.HTMLElement]
    val oldDataset = oldVnode.data.dataset
    val dataset = vnode.data.dataset
    val d = elm.dataset

    oldDataset.foreach { case (key, _) =>
      dataset.get(key) match {
        case None =>
          if (!js.isUndefined(d)) { // TODO: does this make sense?
            d -= key
          } else {
            elm.removeAttribute(
              "data-" + key.replaceAll(CAPS_REGEX, "-$&").toLowerCase()
            )
          }
        case Some(_) => ()
      }
    }

    dataset.foreach { case (key, value) =>
      if (oldDataset.get(key).forall(_ != value)) {
        if (!js.isUndefined(d)) { // TODO: does this make sense?
          d += (key -> value)
        } else {
          elm.setAttribute(
            "data-" + key.replaceAll(CAPS_REGEX, "-$&").toLowerCase(),
            value
          )
        }
      }
    }

  }

}

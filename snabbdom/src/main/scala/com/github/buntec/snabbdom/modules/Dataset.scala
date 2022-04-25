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

package com.github.buntec.snabbdom.modules

import com.github.buntec.snabbdom._
import org.scalajs.dom

object Dataset {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateDataset(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateDataset(oldVNode, vNode)
    })
  )

  private val CAPS_REGEX = "[A-Z]"

  private def updateDataset(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.asInstanceOf[dom.HTMLElement]
    val oldDataset = oldVnode.data.flatMap(_.dataset)
    val dataset = vnode.data.flatMap(_.dataset)
    val d = elm.dataset

    def update(
        oldDataset: Map[String, String],
        dataset: Map[String, String]
    ): Unit = {

      oldDataset.foreach { case (key, _) =>
        dataset.get(key) match {
          case None =>
            if (d != null) { // TODO: does this make sense?
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
          if (d != null) { // TODO: does this make sense?
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

    (oldDataset, dataset) match {
      case (Some(oldDataset), Some(dataset)) if oldDataset != dataset =>
        update(oldDataset, dataset)
      case (Some(oldDataset), None) =>
        update(oldDataset, Map.empty)
      case (None, Some(dataset)) => update(Map.empty, dataset)
      case _                     => ()
    }

  }

}

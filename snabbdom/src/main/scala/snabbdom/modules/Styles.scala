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

import scalajs.js
import org.scalajs.dom

object Styles {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(emptyVNode: VNode, vNode: VNode): Any =
        updateStyle(emptyVNode, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): Any =
        updateStyle(oldVNode, vNode)
    })
  )

  private def updateStyle(oldVnode: VNode, vnode: VNode): Unit = {

    val elm = vnode.elm.get

    def update(
        oldStyle: Map[String, StyleValue],
        style: Map[String, StyleValue]
    ): Unit = {

      oldStyle.foreach { case (name, _) =>
        style.get(name) match {
          case Some(_) =>
          case None =>
            if (name(0) == '-' && name(1) == '-') {
              elm.asInstanceOf[dom.HTMLElement].style.removeProperty(name)
            } else {
              elm
                .asInstanceOf[dom.HTMLElement]
                .style
                .asInstanceOf[js.Dictionary[String]](name) = ""
            }
        }
      }

      style.foreach { case (name, cur) =>
        if (oldStyle.get(name).forall(_ != cur)) {

          if (name(0) == '-' && name(1) == '-') {

            // should be dom.HTMLElement but scoverage bug
            elm.asInstanceOf[js.Dynamic].style.setProperty(name, cur)

          } else {

            elm
              .asInstanceOf[dom.HTMLElement]
              .style
              .asInstanceOf[js.Dictionary[String]](name) = cur

          }
        }
      }

    }

    val oldStyle = oldVnode.data.style
    val style = vnode.data.style

    if (oldStyle != style) {
      update(oldStyle, style)
    }

  }

}

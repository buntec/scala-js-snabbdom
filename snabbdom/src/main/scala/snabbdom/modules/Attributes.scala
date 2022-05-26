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

object Attributes {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(vNode: VNode): Unit =
        updateAttrs(None, vNode)
    }),
    update = Some(new UpdateHook {
      override def apply(oldVNode: VNode, vNode: VNode): VNode = {
        updateAttrs(Some(oldVNode), vNode)
        vNode
      }
    })
  )

  private val xlinkNS = "http://www.w3.org/1999/xlink"
  private val xmlNS = "http://www.w3.org/XML/1998/namespace"

  private def updateAttrs(oldVnode: Option[VNode], vnode: VNode): Unit = {

    val elm = vnode.elm.get.asInstanceOf[dom.Element]

    def update(
        oldAttrs: Map[String, AttrValue],
        attrs: Map[String, AttrValue]
    ) = {
      attrs.foreach { case (key, cur) =>
        val old = oldAttrs.get(key)
        if (old.forall(_ != cur)) {
          if (cur == true) {
            elm.setAttribute(key, "")
          } else if (cur == false) {
            elm.removeAttribute(key)
          } else {
            if (key.charAt(0) != 'x') {
              elm.setAttribute(key, cur.toString)
            } else if (key.length > 3 && key.charAt(3) == ':') {
              elm.setAttributeNS(xmlNS, key, cur.toString)
            } else if (key.length > 5 && key.charAt(5) == ':') {
              elm.setAttributeNS(xlinkNS, key, cur.toString)
            } else {
              elm.setAttribute(key, cur.toString)
            }
          }
        }
      }

      oldAttrs.foreach { case (key, _) =>
        if (!attrs.contains(key)) {
          elm.removeAttribute(key)
        }
      }
    }

    val oldAttrs = oldVnode.map(_.data.attrs).getOrElse(Map.empty)
    val attrs = vnode.data.attrs

    if (oldAttrs != attrs) {
      update(oldAttrs, attrs)
    }

  }

}

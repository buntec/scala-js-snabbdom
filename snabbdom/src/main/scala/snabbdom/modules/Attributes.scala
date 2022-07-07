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

object Attributes {

  val module: Module = Module().copy(
    create = Some(new CreateHook {
      override def apply(vNode: PatchedVNode): Unit = {
        vNode match {
          case elm: PatchedVNode.Element =>
            if (elm.data.attrs.nonEmpty) {
              setAttrs(elm)
            }
          case _ => ()
        }
      }
    }),
    update = Some(new UpdateHook {
      override def apply(
          oldVNode: PatchedVNode,
          vNode: VNode
      ): Unit = {
        (oldVNode, vNode) match {
          case (a: PatchedVNode.Element, b: VNode.Element) =>
            if (a.data.attrs != b.data.attrs) {
              updateAttrs(a, b)
            }
          case _ => ()
        }
      }
    })
  )

  private val xlinkNS = "http://www.w3.org/1999/xlink"
  private val xmlNS = "http://www.w3.org/XML/1998/namespace"

  private def setAttrs(vnode: PatchedVNode.Element): Unit = {
    val elm = vnode.node
    vnode.data.attrs.foreach {
      case (key, AttrValue.BooleanAttrValue(value)) =>
        if (value) {
          elm.setAttribute(key, "")
        } else {
          elm.removeAttribute(key)
        }
      case (key, AttrValue.StringAttrValue(value)) =>
        if (key.charAt(0) != 'x') {
          elm.setAttribute(key, value)
        } else if (key.length > 3 && key.charAt(3) == ':') {
          elm.setAttributeNS(xmlNS, key, value)
        } else if (key.length > 5 && key.charAt(5) == ':') {
          elm.setAttributeNS(xlinkNS, key, value)
        } else {
          elm.setAttribute(key, value)
        }
    }
  }

  private def updateAttrs(
      oldVnode: PatchedVNode.Element,
      vnode: VNode.Element
  ): Unit = {

    val elm = oldVnode.node
    val oldAttrs = oldVnode.data.attrs
    val attrs = vnode.data.attrs

    attrs.foreach { case (key, cur) =>
      if (oldAttrs.get(key).forall(_ != cur)) {
        cur match {
          case AttrValue.BooleanAttrValue(value) =>
            if (value) {
              elm.setAttribute(key, "")
            } else {
              elm.removeAttribute(key)
            }
          case AttrValue.StringAttrValue(value) =>
            if (key.charAt(0) != 'x') {
              elm.setAttribute(key, value)
            } else if (key.length > 3 && key.charAt(3) == ':') {
              elm.setAttributeNS(xmlNS, key, value)
            } else if (key.length > 5 && key.charAt(5) == ':') {
              elm.setAttributeNS(xlinkNS, key, value)
            } else {
              elm.setAttribute(key, value)
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

}

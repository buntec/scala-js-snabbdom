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

object Props {

  val module: Module = Module().copy(
    create = Some((vNode: PatchedVNode) => {
      vNode match {
        case elm: PatchedVNode.Element =>
          setProps(elm)
        case _ => ()
      }
    }),
    update = Some((oldVNode: PatchedVNode, vNode: VNode) => {
      (oldVNode, vNode) match {
        case (a: PatchedVNode.Element, b: VNode.Element) =>
          if (a.data.props != b.data.props) {
            updateProps(a, b)
          }
        case _ => ()
      }
    })
  )

  private def setProps(vnode: PatchedVNode.Element): Unit = {
    vnode match {
      case elm: PatchedVNode.Element =>
        elm.data.props.foreach { case (key, cur) =>
          elm.node.asInstanceOf[js.Dictionary[Any]] += (key -> cur)
        }
    }
  }

  private def updateProps(
      oldVnode: PatchedVNode.Element,
      vnode: VNode.Element
  ): Unit = {

    val elm = oldVnode.node
    val oldProps = oldVnode.data.props
    val props = vnode.data.props

    props.foreach { case (key, cur) =>
      if (oldProps.get(key).forall(_ != cur)) {
        elm.asInstanceOf[js.Dictionary[Any]] += (key -> cur)
      }
    }

    // TODO: remove altogether?
    // This is mostly futile b/c it only removes
    // Own properties while properties of
    // DOM elements are typically inherited.
    // E.g., a prop like `id` cannot be deleted
    // (removing the corresponding (reflected) attribute
    // sets this property to the empty string).
    oldProps.foreach { case (key, _) =>
      if (!props.contains(key)) {
        elm.asInstanceOf[js.Dictionary[Any]] -= key
      }
    }

  }

}

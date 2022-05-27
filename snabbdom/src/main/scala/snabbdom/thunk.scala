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

object thunk {

  def apply(
      sel: String,
      fn: Any => VNode,
      args: Any
  ): VNode = apply(sel, None, fn, args)

  def apply(
      sel: String,
      key: String,
      fn: Any => VNode,
      args: Any
  ): VNode = apply(sel, Some(key), fn, args)

  private def apply(
      sel: String,
      key: Option[String],
      fn: Any => VNode,
      args: Any
  ): VNode = {
    val hook = Hooks().copy(
      init = Some((vNode: VNode) => init0(vNode)),
      prepatch = Some((oldVNode: PatchedVNode, vNode: VNode) =>
        prepatch0(oldVNode, vNode)
      )
    )
    val data =
      VNodeData(key = key, fn = Some(fn), args = Some(args), hook = Some(hook))
    h(sel, data)
  }

  private def init0(thunk: VNode): VNode = {
    val fn = thunk.data.fn.get
    val args = thunk.data.args.get
    val vnode = fn(args)
    thunk.copy(
      children = vnode.children,
      data = vnode.data.copy(fn = Some(fn), args = Some(args)),
      text = vnode.text
    )
  }

  private def prepatch0(oldVnode: PatchedVNode, thunk: VNode): VNode = {
    val old = oldVnode.data
    val cur = thunk.data
    val oldArgs = old.args
    val args = cur.args
    val oldFn = old.fn
    val curFn = cur.fn
    if (oldFn != curFn || oldArgs != args) {
      val vnode = curFn.get(args.get)
      thunk.copy(
        children = vnode.children,
        data = vnode.data.copy(fn = curFn, args = args),
        text = vnode.text
      )
    } else {
      oldVnode.toVNode
    }
  }

}

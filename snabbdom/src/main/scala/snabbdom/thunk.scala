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
      fn: Seq[Any] => VNode,
      args: Seq[Any]
  ): VNode = apply(sel, None, fn, args)

  def apply(
      sel: String,
      key: String,
      fn: Seq[Any] => VNode,
      args: Seq[Any]
  ): VNode = apply(sel, Some(key), fn, args)

  private def apply(
      sel: String,
      key: Option[String],
      fn: Seq[Any] => VNode,
      args: Seq[Any]
  ): VNode = {
    val data = VNodeData.empty
    data.key = key
    data.fn = Some(fn)
    data.args = Some(args)
    data.hook = Some(
      Hooks().copy(
        init = Some((vNode: VNode) => init0(vNode)),
        prepatch =
          Some((oldVNode: VNode, vNode: VNode) => prepatch0(oldVNode, vNode))
      )
    )
    h(sel, data)
  }

  private def init0(thunk: VNode): Unit = {
    val data = thunk.data.get
    val fn = data.fn.get
    val args = data.args.get
    copyToThunk(fn(args), thunk)
  }

  private def prepatch0(oldVnode: VNode, thunk: VNode): Unit = {
    val old = oldVnode.data
    val cur = thunk.data
    val oldArgs = old.flatMap(_.args)
    val args = cur.flatMap(_.args)
    val oldFn = old.flatMap(_.fn)
    val curFn = cur.flatMap(_.fn)
    if (oldFn != curFn || oldArgs != args) {
      copyToThunk(curFn.get(args.get), thunk)
    } else {
      copyToThunk(oldVnode, thunk)
    }
  }

  private def copyToThunk(vnode: VNode, thunk: VNode): Unit = {
    val ns = thunk.data.flatMap(_.ns)
    vnode.data.foreach(_.fn = thunk.data.flatMap(_.fn))
    vnode.data.foreach(_.args = thunk.data.flatMap(_.args))
    thunk.data = vnode.data
    thunk.children = vnode.children
    thunk.text = vnode.text
    thunk.elm = vnode.elm
    ns.foreach(_ => h.addNS(thunk.data.get, thunk.children, thunk.sel))
  }

}

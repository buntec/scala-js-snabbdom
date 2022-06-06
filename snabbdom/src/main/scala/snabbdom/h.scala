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
import snabbdom.PatchedVNode.Comment

object h {

  def comment(content: String): VNode.Comment = VNode.Comment(content)

  def apply(sel: String): VNode.Element = {
    val data =
      if (isSvg(sel)) VNodeData(ns = Some(svgNamespace)) else VNodeData.empty
    VNode.Element(sel, data, Nil)
  }

  def apply(sel: String, data: VNodeData): VNode.Element = {
    val data0 = if (isSvg(sel)) data.copy(ns = Some(svgNamespace)) else data
    VNode.Element(sel, data0, Nil)
  }

  def apply(sel: String, children: List[VNode]): VNode.Element = {
    val data =
      if (isSvg(sel)) VNodeData(ns = Some(svgNamespace)) else VNodeData.empty
    VNode.Element(
      sel,
      data,
      if (isSvg(sel)) children.map(addNS) else children
    )
  }

  def apply(sel: String, text: String): VNode.Element = {
    val data =
      if (isSvg(sel)) VNodeData(ns = Some(svgNamespace)) else VNodeData.empty
    VNode.Element(sel, data, List(VNode.text(text)))
  }

  def apply(sel: String, data: VNodeData, text: String): VNode.Element = {
    val data0 = if (isSvg(sel)) data.copy(ns = Some(svgNamespace)) else data
    VNode.Element(sel, data0, List(VNode.text(text)))
  }

  def apply(
      sel: String,
      data: VNodeData,
      children: List[VNode]
  ): VNode.Element =
    VNode.Element(sel, data, children)

  def apply(sel: String, data: VNodeData, child: VNode): VNode.Element = {
    val data0 = if (isSvg(sel)) data.copy(ns = Some(svgNamespace)) else data
    VNode.Element(
      sel,
      data0,
      List(if (isSvg(sel)) addNS(child) else child)
    )
  }

  private def isSvg(sel: String): Boolean = {
    sel.startsWith("svg") && (sel.length == 3 || sel(3) == '.' || sel(3) == '#')
  }

  private val svgNamespace = "http://www.w3.org/2000/svg"

  private[snabbdom] def addNS(vnode: PatchedVNode): PatchedVNode = {
    vnode match {
      case Comment(_, _)           => vnode
      case PatchedVNode.Text(_, _) => vnode
      case PatchedVNode.Fragment(children, elm) =>
        PatchedVNode.Fragment(children.map(addNS), elm)
      case PatchedVNode.Element(sel, data, children, elm) =>
        PatchedVNode.Element(
          sel,
          data.copy(ns = Some(svgNamespace)),
          if (sel != "foreignObject") children.map(addNS) else children,
          elm
        )
    }
  }

  private[snabbdom] def addNS(vnode: VNode): VNode = {
    vnode match {
      case VNode.Comment(_)         => vnode
      case VNode.Text(_)            => vnode
      case VNode.Fragment(children) => VNode.Fragment(children.map(addNS))
      case VNode.Element(sel, data, children) =>
        VNode.Element(
          sel,
          data.copy(ns = Some(svgNamespace)),
          if (sel != "foreignObject") children.map(addNS) else children
        )
    }
  }

}

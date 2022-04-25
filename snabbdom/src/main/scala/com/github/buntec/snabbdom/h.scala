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

package com.github.buntec.snabbdom

object h {

  type VNodes = Array[VNode]

  def apply(sel: String): VNode = h(sel, None, None, None)

  def apply(sel: String, data: VNodeData): VNode = {
    apply(sel, Some(data), None, None)
  }

  def apply(sel: String, children: Array[VNode]): VNode = {
    apply(sel, None, Some(children), None)
  }

  def apply(sel: String, text: String): VNode = {
    apply(sel, None, None, Some(text))
  }

  def apply(sel: String, data: VNodeData, text: String): VNode = {
    apply(sel, Some(data), None, Some(text))
  }

  def apply(sel: String, data: VNodeData, children: Array[VNode]): VNode = {
    apply(sel, Some(data), Some(children), None)
  }

  def apply(sel: String, data: VNodeData, child: VNode): VNode = {
    apply(sel, Some(data), Some(Array(child)), None)
  }

  private def apply(
      sel: String,
      data: Option[VNodeData],
      children: Option[Array[VNode]],
      text: Option[String]
  ): VNode = {
    if (
      sel(0) == 's' && sel(1) == 'v' && sel(2) == 'g' &&
      (sel.length == 3 || sel(3) == '.' || sel(3) == '#')
    ) {
      addNS(VNodeData.empty, children, Some(sel))
    }
    VNode.create(Some(sel), data, children, text, None)
  }

  private[snabbdom] def addNS(
      data: VNodeData,
      children: Option[Array[VNode]],
      sel: Option[String]
  ): Unit = {
    data.ns = Some("http://www.w3.org/2000/svg")
    if (sel.forall(_ != "foreignObject")) {
      children.foreach {
        _.map { child =>
          child.data.foreach(data => addNS(data, child.children, child.sel))
        }
      }
    }
  }

}

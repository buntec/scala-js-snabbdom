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

sealed trait VNode

object VNode {

  final case class Text(content: String) extends VNode

  final case class Element(sel: String, data: VNodeData, children: List[VNode])
      extends VNode

  final case class Comment(content: String) extends VNode

  val empty: VNode = Text("")

  def text(content: String): VNode = Text(content)

  def element(sel: String, data: VNodeData, children: List[VNode]): VNode =
    Element(sel, data, children)

  def comment(content: String): VNode = Comment(content)

  implicit def fromString(s: String): VNode = text(s)

  implicit class VNodeOps(val vnode: VNode) extends AnyVal {

    def prettyPrint: String = VNode.prettyPrint(vnode)

    def key: Option[String] = VNode.key(vnode)

  }

  private def key(vnode: VNode): Option[String] = vnode match {
    case Text(_)             => None
    case Element(_, data, _) => data.key
    case Comment(_)          => None
  }

  // TODO: include attributes
  private def prettyPrint(vnode: VNode): String = {
    def go(indent: Int, vnode: VNode): String = {
      vnode match {
        case Text(content) => s"${" " * indent}$content"
        case Element(sel, data, children) =>
          s"""|${" " * indent}<$sel${data.key.fold("")(key => s" key=$key")}>
              |${children.map(go(indent + 2, _)).mkString("\n")}
              |${" " * indent}</$sel>""".stripMargin
        case Comment(content) => s"""<!--${content}-->"""
      }
    }
    go(0, vnode)
  }

  private[snabbdom] def applyInitHook(vnode: VNode): VNode = vnode match {
    case Text(_)             => vnode
    case Element(_, data, _) => data.hook.flatMap(_.init).fold(vnode)(_(vnode))
    case Comment(_)          => vnode
  }

}

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

import org.scalajs.dom

import scala.collection.mutable

object toVNode {

  def apply(node: dom.Node, domApi: Option[DomApi] = None): PatchedVNode = {

    val api = domApi.getOrElse(DomApi.apply)

    if (api.isElement(node)) {
      val elm = node.asInstanceOf[dom.Element]
      val id = Option(elm.id).filter(_.nonEmpty).fold("")("#" + _)
      val cn = Option(elm.getAttribute("class")).getOrElse("")
      val c = if (cn != "") "." + cn.split(" ").mkString(".") else ""
      val sel = api.tagName(elm).toLowerCase + id + c
      val attrs = mutable.Map.empty[String, String]
      val datasets = mutable.Map.empty[String, String]

      val children = new mutable.ArrayBuffer[PatchedVNode]
      val elmAttrs = elm.attributes
      val elmChildren = elm.childNodes
      elmAttrs.foreach { case (_, attr) =>
        val name = attr.nodeName
        if (name.startsWith("data-")) {
          datasets += name.drop(5) -> Option(attr.nodeValue).getOrElse("")
        } else if (name != "id" && name != "class") {
          attrs += name -> Option(attr.nodeValue).getOrElse("")
        }
      }

      elmChildren.foreach { childNode =>
        children.append(toVNode(childNode, domApi))
      }

      val data =
        VNodeData(
          attrs = attrs.toMap.map { case (key, value) =>
            key -> AttrValue(value)
          },
          dataset = if (datasets.nonEmpty) datasets.toMap else Map.empty
        )

      val vnode = PatchedVNode.Element(
        sel,
        data,
        children.toList,
        node.asInstanceOf[dom.Element]
      )

      if (
        sel.startsWith("svg") && (sel.length == 3 || sel(3) == '.' || sel(
          3
        ) == '#')
      ) {
        h.addNS(vnode)
      } else {
        vnode
      }

    } else if (api.isText(node)) {
      val text = api.getTextContent(node).getOrElse("")
      PatchedVNode.text(text, node.asInstanceOf[dom.Text])
    } else if (api.isComment(node)) {
      val text = api.getTextContent(node).getOrElse("")
      PatchedVNode.comment(
        text,
        node.asInstanceOf[dom.Comment]
      )
    } else if (api.isDocumentFragement(node)) {
      val children = new mutable.ArrayBuffer[PatchedVNode]
      val elmChildren = node.childNodes
      elmChildren.foreach { childNode =>
        children.append(toVNode(childNode, domApi))
      }
      PatchedVNode.fragment(
        children.toList,
        node.asInstanceOf[dom.DocumentFragment]
      )
    } else {
      throw new IllegalArgumentException(s"Unexpected node type: $node")
    }

  }

}

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
      val (sel, ns) = elementIdentity(elm, api)
      val attrs = mutable.Map.empty[String, String]
      val datasets = mutable.Map.empty[String, String]

      val children = new mutable.ArrayBuffer[PatchedVNode]
      val elmAttrs = elm.attributes
      val elmChildren = elm.childNodes
      elmAttrs.foreach { case (_, attr) =>
        val name = attr.nodeName
        if (name.startsWith("data-")) {
          datasets += datasetKey(name) -> Option(attr.nodeValue).getOrElse("")
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
          dataset = if (datasets.nonEmpty) datasets.toMap else Map.empty,
          ns = ns
        )

      PatchedVNode.Element(
        sel,
        data,
        children.toList,
        elm
      )

    } else if (api.isText(node)) {
      val text = api.getTextContent(node).getOrElse("")
      PatchedVNode.text(text, node.asInstanceOf[dom.Text])
    } else if (api.isComment(node)) {
      val text = api.getTextContent(node).getOrElse("")
      PatchedVNode.comment(
        text,
        node.asInstanceOf[dom.Comment]
      )
    } else {
      throw new IllegalArgumentException(s"Unexpected node type: $node")
    }

  }

  private[snabbdom] def elementIdentity(
      elm: dom.Element,
      api: DomApi
  ): (String, Option[String]) = {
    val namespace = elm.namespaceURI
    val html = namespace == "http://www.w3.org/1999/xhtml"
    // Elements without a namespace (`createElementNS(null, ...)`) map to
    // `Some("")`: `createElementNS("", tag)` recreates exactly such an
    // element, whereas `None` would create an HTML element instead.
    val ns = if (html) None else Some(Option(namespace).getOrElse(""))
    val tag = api.tagName(elm)
    val id = Option(elm.id).filter(_.nonEmpty).fold("")("#" + _)
    val classes =
      Option(elm.getAttribute("class")).fold(List.empty[String])(classNames)
    val suffix = if (classes.nonEmpty) classes.mkString(".", ".", "") else ""
    ((if (html) tag.toLowerCase else tag) + id + suffix, ns)
  }

  // Splits a class attribute on HTML whitespace (space, tab, LF, FF, CR).
  private def classNames(cn: String): List[String] =
    cn.split("[\\t\\r\\n\\f ]+").toList.filter(_.nonEmpty)

  // Transforms a `data-` attribute name into a dataset property key, see
  // https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/dataset#name_conversion
  private def datasetKey(attributeName: String): String = {
    val name = attributeName.drop(5)
    val sb = new java.lang.StringBuilder(name.length)
    var i = 0
    while (i < name.length) {
      val ch = name.charAt(i)
      if (
        ch == '-' && i + 1 < name.length && name.charAt(i + 1) >= 'a' && name
          .charAt(i + 1) <= 'z'
      ) {
        sb.append(name.charAt(i + 1).toUpper)
        i += 2
      } else {
        sb.append(ch)
        i += 1
      }
    }
    sb.toString
  }

}

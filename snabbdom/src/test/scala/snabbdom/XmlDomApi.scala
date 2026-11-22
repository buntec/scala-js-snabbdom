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

// A `DomApi` that creates nodes in `doc`, typically an XML document, where
// `createElement` produces elements without a namespace.
object XmlDomApi {

  def apply(doc: dom.Document): DomApi = {
    val base = DomApi.apply
    new DomApi {
      def createElement(tagName: String): dom.HTMLElement =
        doc.createElement(tagName).asInstanceOf[dom.HTMLElement]
      def createElementNS(ns: String, name: String): dom.Element =
        doc.createElementNS(ns, name)
      def createDocumentFragment: dom.DocumentFragment =
        doc.createDocumentFragment()
      def createTextNode(text: String): dom.Text = doc.createTextNode(text)
      def createComment(text: String): dom.Comment = doc.createComment(text)
      def insertBefore(p: dom.Node, n: dom.Node, r: Option[dom.Node]): Unit =
        base.insertBefore(p, n, r)
      def removeChild(node: dom.Node, child: dom.Node): Unit =
        base.removeChild(node, child)
      def appendChild(node: dom.Node, child: dom.Node): Unit =
        base.appendChild(node, child)
      def parentNode(node: dom.Node): Option[dom.Node] = base.parentNode(node)
      def nextSibling(node: dom.Node): Option[dom.Node] =
        base.nextSibling(node)
      def tagName(elm: dom.Element): String = base.tagName(elm)
      def setTextContent(node: dom.Node, text: Option[String]): Unit =
        base.setTextContent(node, text)
      def getTextContent(node: dom.Node): Option[String] =
        base.getTextContent(node)
      def isElement(node: dom.Node): Boolean = base.isElement(node)
      def isText(node: dom.Node): Boolean = base.isText(node)
      def isComment(node: dom.Node): Boolean = base.isComment(node)
      def isDocumentFragement(node: dom.Node): Boolean =
        base.isDocumentFragement(node)
    }
  }

  def svgDocument(): dom.Document =
    dom.document.implementation
      .createDocument("http://www.w3.org/2000/svg", "svg", null)

}

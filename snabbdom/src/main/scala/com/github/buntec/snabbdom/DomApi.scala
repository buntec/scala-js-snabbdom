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

import org.scalajs.dom

trait DomApi {

  def createElement(
      tagName: String
  ): dom.HTMLElement

  def createElementNS(
      namespaceURI: String,
      qualifiedName: String
  ): dom.Element

  def createDocumentFragment: dom.DocumentFragment

  def createTextNode(text: String): dom.Text

  def createComment(text: String): dom.Comment

  def insertBefore(
      parentNode: dom.Node,
      newNode: dom.Node,
      referenceNode: Option[dom.Node]
  ): Unit

  def removeChild(node: dom.Node, child: dom.Node): Unit

  def appendChild(node: dom.Node, child: dom.Node): Unit

  def parentNode(node: dom.Node): Option[dom.Node]

  def nextSibling(node: dom.Node): Option[dom.Node]

  def tagName(elm: dom.Element): String

  def setTextContext(node: dom.Node, text: Option[String]): Unit

  def getTextContext(node: dom.Node): Option[String]

  def isElement(node: dom.Node): Boolean // type guard

  def isText(node: dom.Node): Boolean // type guard

  def isComment(node: dom.Node): Boolean // type guard

  def isDocumentFragement(node: dom.Node): Boolean // type guard

}

object DomApi {

  def apply: DomApi = new DomApi {

    override def createElement(tagName: String): dom.HTMLElement =
      dom.document
        .createElement(tagName)
        .asInstanceOf[dom.HTMLElement] // TODO: check cast

    override def createElementNS(
        namespaceURI: String,
        qualifiedName: String
    ): dom.Element = dom.document.createElementNS(namespaceURI, qualifiedName)

    override def createDocumentFragment: dom.DocumentFragment =
      dom.document.createDocumentFragment()

    override def createTextNode(text: String): dom.Text =
      dom.document.createTextNode(text)

    override def createComment(text: String): dom.Comment =
      dom.document.createComment(text)

    override def insertBefore(
        parentNode: dom.Node,
        newNode: dom.Node,
        referenceNode: Option[dom.Node]
    ): Unit = {
      parentNode.insertBefore(newNode, referenceNode.getOrElse(null))
      ()
    }

    override def removeChild(node: dom.Node, child: dom.Node): Unit = {
      node.removeChild(child)
      ()
    }

    override def appendChild(node: dom.Node, child: dom.Node): Unit = {
      node.appendChild(child)
      ()
    }

    override def parentNode(node: dom.Node): Option[dom.Node] =
      Option(node.parentNode)

    override def nextSibling(node: dom.Node): Option[dom.Node] =
      Option(node.nextSibling)

    override def tagName(elm: dom.Element): String = elm.tagName

    override def setTextContext(node: dom.Node, text: Option[String]): Unit = {
      node.textContent = text.getOrElse(null)
    }

    override def getTextContext(node: dom.Node): Option[String] =
      Option(node.textContent)

    override def isElement(node: dom.Node): Boolean =
      node.nodeType == 1

    override def isText(node: dom.Node): Boolean =
      node.nodeType == 3

    override def isComment(node: dom.Node): Boolean =
      node.nodeType == 8

    override def isDocumentFragement(node: dom.Node): Boolean =
      node.nodeType == 1

  }

}

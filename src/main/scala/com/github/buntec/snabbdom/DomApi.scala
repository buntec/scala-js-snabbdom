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

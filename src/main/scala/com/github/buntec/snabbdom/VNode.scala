package com.github.buntec.snabbdom

import org.scalajs.dom._

import scala.scalajs.js

case class VNode(
    sel: Option[String] = None,
    data: Option[VNodeData] = None,
    children: Option[Array[VNode]] = None,
    elm: Option[Node] = None,
    text: Option[String] = None,
    key: Option[Key] = None
)

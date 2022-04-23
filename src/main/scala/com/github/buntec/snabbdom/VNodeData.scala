package com.github.buntec.snabbdom

import org.scalajs.dom._

import scala.scalajs.js

case class VNodeData(
    props: Option[Map[String, PropValue]],
    attrs: Option[Map[String, AttrValue]],
    `class`: Option[Map[String, Boolean]],
    style: Option[Map[String, StyleValue]],
    dataset: Option[Map[String, String]],
    key: Option[KeyValue],
    ns: Option[String]
)

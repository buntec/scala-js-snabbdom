package com.github.buntec.snabbdom

class VNodeData(
    var props: Option[Map[String, PropValue]],
    var attrs: Option[Map[String, AttrValue]],
    var `class`: Option[Map[String, ClassValue]],
    var style: Option[Map[String, StyleValue]],
    var dataset: Option[Map[String, String]],
    var hook: Option[Hooks],
    var key: Option[KeyValue],
    var ns: Option[String],
    var is: Option[String]
)

object VNodeData {

  def empty =
    new VNodeData(None, None, None, None, None, None, None, None, None)

}

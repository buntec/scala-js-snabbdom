package com.github.buntec.snabbdom

class VNodeData(
    var props: Option[Map[String, PropValue]],
    var attrs: Option[Map[String, AttrValue]],
    var classes: Option[Map[String, ClassValue]],
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

  def builder = new Builder()

  class Builder() {
    private val data = empty

    def build: VNodeData = data

    def withProps(props: (String, PropValue)*): Builder = {
      data.props = Some(props.toMap)
      this
    }

    def withAttrs(attrs: (String, AttrValue)*): Builder = {
      data.attrs = Some(attrs.toMap)
      this
    }

    def withClasses(classes: (String, ClassValue)*): Builder = {
      data.classes = Some(classes.toMap)
      this
    }

    def withStyle(style: (String, StyleValue)*): Builder = {
      data.style = Some(style.toMap)
      this
    }

  }

}

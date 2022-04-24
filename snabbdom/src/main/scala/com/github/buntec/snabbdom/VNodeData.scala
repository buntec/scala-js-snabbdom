package com.github.buntec.snabbdom

import org.scalajs.dom

class VNodeData(
    var props: Option[Map[String, PropValue]],
    var attrs: Option[Map[String, AttrValue]],
    var classes: Option[Map[String, ClassValue]],
    var style: Option[Map[String, StyleValue]],
    var dataset: Option[Map[String, String]],
    var on: Option[Map[String, dom.Event => Unit]],
    var hook: Option[Hooks],
    var key: Option[String],
    var ns: Option[String], // for SVG
    var fn: Option[Seq[Any] => VNode], // for thunks
    var args: Option[Seq[Any]], // for thunks
    var is: Option[String]
)

object VNodeData {

  def empty =
    new VNodeData(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

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

    def withOn(on: (String, (dom.Event) => Unit)*): Builder = {
      data.on = Some(on.toMap)
      this
    }

  }

}

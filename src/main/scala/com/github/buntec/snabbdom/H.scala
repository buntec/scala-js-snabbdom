package com.github.buntec.snabbdom

object H {

  type VNodes = Array[VNode]

  def addNS(
      data: VNodeData,
      children: Option[Array[VNode]],
      sel: Option[String]
  ): Unit = {
    data.ns = Some("http://www.w3.org/2000/svg")
    if (sel.forall(_ != "foreignObject")) {
      children.foreach {
        _.map { child =>
          child.data.foreach(data => addNS(data, child.children, child.sel))
        }
      }
    }
  }

  def h(sel: String): VNode = h(sel, None, None, None)

  def h(sel: String, data: VNodeData): VNode = {
    h(sel, Some(data), None, None)
  }

  def h(sel: String, children: Array[VNode]): VNode = {
    h(sel, None, Some(children), None)
  }

  def h(sel: String, data: VNodeData, children: Array[VNode]): VNode = {
    h(sel, Some(data), Some(children), None)
  }

  def h(
      sel: String,
      data: Option[VNodeData],
      children: Option[Array[VNode]],
      text: Option[String]
  ): VNode = {
    if (
      sel(0) == 's' && sel(1) == 'v' && sel(2) == 'g' &&
      (sel.length == 3 || sel(3) == '.' || sel(3) == '#')
    ) {
      addNS(VNodeData.empty, children, Some(sel))
    }
    VNode.create(Some(sel), data, children, text, None)
  }

}

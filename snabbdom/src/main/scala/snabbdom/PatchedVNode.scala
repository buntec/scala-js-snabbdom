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

package snabbdom

import org.scalajs.dom
import scalajs.js

/** A `VNode` that has been patched into the DOM. */
sealed trait PatchedVNode {
  def node: dom.Node // the corresponding node in the DOM
  def toVNode: VNode
}

object PatchedVNode {

  def key(vnode: PatchedVNode): Option[String] = vnode match {
    case Element(_, data, _, _) => data.key
    case _                      => None
  }

  // This is needed for inserting a node before a fragment b/c the
  // fragment itself is not actually part of the DOM so we have to find the
  // first child that is not a fragment and insert before that.
  private[snabbdom] def firstNonFragmentNode(
      vnode: PatchedVNode
  ): Option[PatchedVNode] =
    vnode match {
      case Fragment(_, children, _) =>
        children.headOption.flatMap(firstNonFragmentNode)
      case _ => Some(vnode)
    }

  final case class Element(
      sel: String,
      data: VNodeData,
      children: List[PatchedVNode],
      node: dom.Element
  ) extends PatchedVNode {

    private def handleEvent(event: dom.Event): Unit = {
      val name = event.`type`
      data.on.get(name).foreach { handler =>
        handler.cbs.foreach(_(event, this.toVNode))
      }
    }

    private[snabbdom] lazy val listener: js.Function1[dom.Event, Unit] =
      handleEvent _

    override def toVNode: VNode =
      VNode.Element(sel, data, children.map(_.toVNode))

  }

  def element(
      sel: String,
      data: VNodeData,
      children: List[PatchedVNode],
      node: dom.Element
  ): PatchedVNode =
    Element(
      sel,
      data,
      children,
      node
    )

  final case class Text(
      content: String,
      node: dom.Text
  ) extends PatchedVNode {

    override def toVNode: VNode = VNode.Text(content)

  }

  def text(content: String, node: dom.Text): PatchedVNode = Text(content, node)

  final case class Fragment(
      parent: dom.Node,
      children: List[PatchedVNode],
      node: dom.DocumentFragment
  ) extends PatchedVNode {

    override def toVNode: VNode = VNode.Fragment(children.map(_.toVNode))

  }

  def fragment(
      parent: dom.Node,
      children: List[PatchedVNode],
      node: dom.DocumentFragment
  ): PatchedVNode = Fragment(parent, children, node)

  final case class Comment(content: String, node: dom.Comment)
      extends PatchedVNode {

    override def toVNode: VNode = VNode.comment(content)

  }

  def comment(content: String, node: dom.Comment): PatchedVNode =
    PatchedVNode.Comment(content, node)

}

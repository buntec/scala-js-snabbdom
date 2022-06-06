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

/** A `VNode` that has been patched into the DOM. */
final case class PatchedVNode private[snabbdom] (
    sel: Option[String],
    data: VNodeData,
    children: List[PatchedVNode],
    text: Option[String],
    elm: dom.Node, // the corresponding node in the DOM - can't be `dom.Element` unfortunately b/c of fragments
    listener: Option[
      Listener
    ] // this is an optimization that allows re-using event listeners
) {

  def toVNode: VNode = VNode(sel, data, children.map(_.toVNode), text)

  private[snabbdom] def isTextNode: Boolean =
    sel.isEmpty && children.isEmpty && text.isDefined

}

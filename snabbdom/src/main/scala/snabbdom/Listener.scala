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

class Listener(var vnode: VNode) {

  def handleEvent(event: dom.Event): Unit = {
    val name = event.`type`
    vnode match {
      case VNode.Element(_, data, _) =>
        data.on.get(name).foreach { handler =>
          handler.cbs.foreach(cb => cb(event, vnode))
        }
      case VNode.Text(_)     => ()
      case VNode.Fragment(_) => ()
      case VNode.Comment(_)  => ()
    }
  }

  /* This is required because calls to `removeEventListener`
   * need a stable reference to the previously registered
   * listener and using `handleEvent` directly would result
   * in a new implicit conversion to `js.Function1` every time.
   */
  private[snabbdom] val jsFun: js.Function1[dom.Event, Unit] = handleEvent _

}

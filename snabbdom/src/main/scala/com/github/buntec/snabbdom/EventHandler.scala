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

package com.github.buntec.snabbdom

import org.scalajs.dom

class EventHandler(
    val cbs: Seq[(dom.Event, VNode) => Unit]
)

object EventHandler {

  def apply(cbs: (dom.Event => Unit)*): EventHandler = new EventHandler(
    cbs.map { cb => (ev: dom.Event, _: VNode) => cb(ev) }
  )
  def usingVNode(cbs: ((dom.Event, VNode) => Unit)*): EventHandler =
    new EventHandler(cbs)

  // Not sure about this...
  implicit def fromFunction1(cb: dom.Event => Unit): EventHandler = apply(cb)
  implicit def fromFunction2(cb: (dom.Event, VNode) => Unit): EventHandler =
    usingVNode(cb)

}

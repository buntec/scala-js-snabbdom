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

/*
 * Copyright (c) 2015 Simon Friis Vindum
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

package com.github.buntec.snabbdom

import com.github.buntec.snabbdom.modules._

import org.scalajs.dom

class EventListenersSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(
    Seq(
      EventListeners.module
    )
  )

  vnode0.test("attaches click event handler to element") { vnode0 =>
    val result = List.newBuilder[dom.Event]
    val clicked = (ev: dom.Event) => { result += ev; () }
    val vnode = h(
      "div",
      VNodeData.builder.withOn("click" -> clicked).build,
      Array(h("a", "Click my parent"))
    )
    val elm = patch(vnode0, vnode).elm.get
    elm.asInstanceOf[dom.HTMLElement].click()
    assertEquals(result.result().length, 1)
  }

  // wouldn't a better name be "detaches old listener when element is the same"
  vnode0.test("does not attach new listener") { vnode0 =>
    val result = List.newBuilder[Int]
    val vnode1 = h(
      "div",
      VNodeData.builder.withOn("click" -> (_ => result += 1)).build,
      Array(h("a", "Click my parent"))
    )
    val vnode2 = h(
      "div",
      VNodeData.builder.withOn("click" -> (_ => result += 2)).build,
      Array(h("a", "Click my parent"))
    )
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
    elm1.click()
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
    elm2.click()
    val result0 = result.result()
    println(result0) // TODO: prints (1, 1)
    assertEquals(result0, List(1, 2))
  }

}

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

package snabbdom

import snabbdom.modules._

import org.scalajs.dom
import scala.collection.mutable.ListBuffer

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
    val clicked = EventHandler((ev: dom.Event) => result += ev)
    val vnode = h(
      "div",
      VNodeData(on = Map("click" -> clicked)),
      List(h("a", "Click my parent"))
    )
    val elm = patch(vnode0, vnode).node
    elm.asInstanceOf[dom.HTMLElement].click()
    assertEquals(result.result().length, 1)
  }

  // wouldn't a better name be "detaches old listener when element is the same"
  vnode0.test("does not attach new listener") { vnode0 =>
    val result = List.newBuilder[Int]
    val vnode1 = h(
      "div",
      VNodeData(on =
        Map("click" -> EventHandler((_: dom.Event) => result += 1))
      ),
      List(h("a", "Click my parent"))
    )
    val vnode2 = h(
      "div",
      VNodeData(on = Map("click" -> EventHandler(_ => result += 2))),
      List(h("a", "Click my parent"))
    )
    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.node.asInstanceOf[dom.HTMLElement]
    elm1.click()
    val elm2 = patch(vnode1p, vnode2).node.asInstanceOf[dom.HTMLElement]
    elm2.click()
    val result0 = result.result()
    assertEquals(result0, List(1, 2))
  }

  vnode0.test("detach attached click event handler to element") { vnode0 =>
    val result = ListBuffer[dom.Event]()
    val clicked = (ev: dom.Event) => {
      result += ev
      ()
    }
    val vnode1 = h(
      "div",
      VNodeData(on = Map("click" -> clicked)),
      List(h("a", "Click my parent"))
    )
    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.node.asInstanceOf[dom.HTMLElement]
    elm1.click()
    assertEquals(result.length, 1)
    val vnode2 = h("div", VNodeData(), List(h("a", "Click my parent")))
    val elm2 = patch(vnode1p, vnode2).node.asInstanceOf[dom.HTMLElement]
    elm2.click()
    assertEquals(result.length, 1)
  }

  vnode0.test("multiple event handlers for same event on same element") {
    vnode0 =>
      var called = 0
      val clicked = (ev: dom.Event, vnode: VNode) => {
        called += 1
        assert(ev.target != null)
        val div = vnode.asInstanceOf[VNode.Element]
        assertEquals(div.sel, "div")
      }

      val vnode1 = h(
        "div",
        VNodeData(on =
          Map("click" -> EventHandler.usingVNode(clicked, clicked, clicked))
        ),
        List(h("a", "Click my parent"))
      )
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.node.asInstanceOf[dom.HTMLElement]
      elm1.click()
      assertEquals(called, 3)
      val vnode2 = h(
        "div",
        VNodeData(on =
          Map("click" -> EventHandler.usingVNode(clicked, clicked))
        ),
        List(h("a", "Click my parent"))
      )
      val elm2 = patch(vnode1p, vnode2).node.asInstanceOf[dom.HTMLElement]
      elm2.click()
      assertEquals(called, 5)
  }

  vnode0.test("access to virtual node in event handler") { vnode0 =>
    val result = ListBuffer[VNode]()
    val clicked = (_: dom.Event, vnode: VNode) => {
      result += vnode
      ()
    }
    val vnode1 = h(
      "div",
      VNodeData(on = Map("click" -> clicked)),
      List(h("a", "Click my parent"))
    )
    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.node.asInstanceOf[dom.HTMLElement]
    elm1.click()
    assertEquals(result.length, 1)
    assertEquals(result(0), vnode1p.toVNode)
  }

  vnode0.test("shared handlers in parent and child nodes") { vnode0 =>
    val result = ListBuffer[dom.Event]()
    val clicked = (ev: dom.Event) => {
      result += ev
      ()
    }
    val vnode1 = h(
      "div",
      VNodeData(on = Map("click" -> clicked)),
      List(
        h(
          "a",
          VNodeData(on = Map("click" -> clicked)),
          "Click my parent"
        )
      )
    )
    val elm1 = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLDivElement]
    elm1.click()
    assertEquals(result.length, 1)
    elm1.firstChild.asInstanceOf[dom.HTMLElement].click()
    assertEquals(result.length, 3)
  }

  vnode0.test("detaches listeners when element is destroyed") { vnode0 =>
    var count = 0
    val handler = EventHandler((_: dom.Event) => count += 1)
    val vnode1 = h(
      "div",
      List(h("button", VNodeData(on = Map("click" -> handler)), "Click"))
    )
    val vnode2 = h("div")
    val p1 = patch(vnode0, vnode1)
    val button = p1.node.childNodes(0).asInstanceOf[dom.HTMLElement]
    button.click()
    assertEquals(count, 1)
    patch(p1, vnode2)
    button.click()
    assertEquals(count, 1)
  }

  vnode0.test("uses the latest handler after patching") { vnode0 =>
    val calls = ListBuffer.empty[String]
    val vnode1 = h(
      "div",
      VNodeData(on =
        Map("click" -> EventHandler((_: dom.Event) => calls += "first"))
      )
    )
    val vnode2 = h(
      "div",
      VNodeData(on =
        Map("click" -> EventHandler((_: dom.Event) => calls += "second"))
      )
    )
    val p1 = patch(vnode0, vnode1)
    val elm = p1.node.asInstanceOf[dom.HTMLElement]
    elm.click()
    patch(p1, vnode2)
    elm.click()
    assertEquals(calls.toList, List("first", "second"))
  }

  vnode0.test("retains listener order and supplies the latest vnode") { root =>
    val calls = ListBuffer.empty[String]
    val handler = EventHandler.usingVNode((_: dom.Event, vnode: VNode) => {
      calls += vnode
        .asInstanceOf[VNode.Element]
        .children
        .head
        .asInstanceOf[VNode.Text]
        .content
      ()
    })
    val data = VNodeData(on = Map("click" -> handler))
    val first = patch(root, h("div", data, "first"))
    val element = first.node.asInstanceOf[dom.HTMLElement]
    element.addEventListener(
      "click",
      (_: dom.Event) => { calls += "native"; () }
    )

    val second = patch(first, h("div", data, "second"))
    element.click()
    assertEquals(calls.toList, List("second", "native"))

    val removed = patch(second, h("div", "third"))
    element.click()
    patch(removed, h("div", data, "fourth"))
    element.click()
    assertEquals(
      calls.toList,
      List("second", "native", "native", "native", "fourth")
    )
  }

  vnode0.test("adds and removes listeners for different events") { vnode0 =>
    val calls = ListBuffer.empty[String]
    val click = EventHandler((_: dom.Event) => calls += "click")
    val focus = EventHandler((_: dom.Event) => calls += "custom")
    val vnode1 = h("div", VNodeData(on = Map("click" -> click)))
    val vnode2 = h("div", VNodeData(on = Map("custom" -> focus)))
    val p1 = patch(vnode0, vnode1)
    val elm = p1.node.asInstanceOf[dom.HTMLElement]
    patch(p1, vnode2)
    elm.click()
    elm.dispatchEvent(new dom.Event("custom"))
    assertEquals(calls.toList, List("custom"))
  }

  vnode0.test("patching twice from a stale vnode keeps one listener") {
    vnode0 =>
      var calls = 0
      def vnode(label: String) = h(
        "button",
        VNodeData(on =
          Map("click" -> EventHandler((_: dom.Event) => calls += 1))
        ),
        label
      )
      val p0 = patch(vnode0, vnode("a"))
      patch(p0, vnode("b"))
      patch(p0, vnode("c"))
      p0.node.asInstanceOf[dom.HTMLElement].click()
      assertEquals(calls, 1)
  }

  vnode0.test("patching from a copied vnode keeps one listener") { vnode0 =>
    var calls = 0
    def vnode(label: String) = h(
      "button",
      VNodeData(on =
        Map("click" -> EventHandler((_: dom.Event) => calls += 1))
      ),
      label
    )
    val p = patch(vnode0, vnode("a")).asInstanceOf[PatchedVNode.Element]
    patch(p.copy(), vnode("b"))
    p.node.asInstanceOf[dom.HTMLElement].click()
    assertEquals(calls, 1)
  }

  vnode0.test("keeps listener state separate from DOM properties") { vnode0 =>
    val patchWithProps = init(Seq(Props.module, EventListeners.module))
    var calls = 0
    def vnode(label: String) = h(
      "button",
      VNodeData(
        props = Map("__snabbdomListener" -> "app"),
        on = Map("click" -> EventHandler((_: dom.Event) => calls += 1))
      ),
      label
    )
    val first = patchWithProps(vnode0, vnode("a"))
    val second = patchWithProps(first, vnode("b"))
    second.node.asInstanceOf[dom.HTMLElement].click()
    assertEquals(calls, 1)
  }

}

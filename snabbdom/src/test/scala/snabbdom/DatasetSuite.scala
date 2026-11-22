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

import org.scalajs.dom
import snabbdom.modules.Dataset

class DatasetSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(Seq(Dataset.module))

  vnode0.test("is set on initial element creation") { vnode0 =>
    val elm = patch(
      vnode0,
      h("div", VNodeData(dataset = Map("foo" -> "foo")))
    ).node.asInstanceOf[dom.HTMLElement]
    assertEquals(elm.dataset("foo"), "foo")
  }

  vnode0.test("updates dataset") { vnode0 =>
    val vnode1 =
      h("i", VNodeData(dataset = Map("foo" -> "foo", "bar" -> "bar")))
    val vnode2 =
      h("i", VNodeData(dataset = Map("baz" -> "baz", "bar" -> "qux")))
    val p1 = patch(vnode0, vnode1)
    val elm = p1.node.asInstanceOf[dom.HTMLElement]
    assertEquals(elm.dataset("foo"), "foo")
    assertEquals(elm.dataset("bar"), "bar")
    patch(p1, vnode2)
    assert(!elm.dataset.contains("foo"))
    assertEquals(elm.dataset("bar"), "qux")
    assertEquals(elm.dataset("baz"), "baz")
    assert(!elm.hasAttribute("data-foo"))
  }

  vnode0.test("can be memoized") { vnode0 =>
    val cachedDataset = Map("foo" -> "foo", "bar" -> "bar")
    val vnode1 = h("i", VNodeData(dataset = cachedDataset))
    val vnode2 = h("i", VNodeData(dataset = cachedDataset))
    val p1 = patch(vnode0, vnode1)
    val elm = p1.node.asInstanceOf[dom.HTMLElement]
    assertEquals(elm.dataset("foo"), "foo")
    assertEquals(elm.dataset("bar"), "bar")
    patch(p1, vnode2)
    assertEquals(elm.dataset("foo"), "foo")
    assertEquals(elm.dataset("bar"), "bar")
  }

  vnode0.test("handles string conversions") { vnode0 =>
    val vnode1 = h(
      "i",
      VNodeData(dataset =
        Map(
          "empty" -> "",
          "dash" -> "-",
          "dashed" -> "foo-bar",
          "camel" -> "fooBar",
          "integer" -> "0",
          "float" -> "0.1"
        )
      )
    )
    val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLElement]
    assertEquals(elm.getAttribute("data-empty"), "")
    assertEquals(elm.getAttribute("data-dash"), "-")
    assertEquals(elm.getAttribute("data-dashed"), "foo-bar")
    assertEquals(elm.getAttribute("data-camel"), "fooBar")
    assertEquals(elm.getAttribute("data-integer"), "0")
    assertEquals(elm.getAttribute("data-float"), "0.1")
  }

  vnode0.test("maps camelCase keys to kebab-case attributes") { vnode0 =>
    val vnode1 = h("i", VNodeData(dataset = Map("fooBarBaz" -> "x")))
    val vnode2 = h("i")
    val p1 = patch(vnode0, vnode1)
    val elm = p1.node.asInstanceOf[dom.HTMLElement]
    assertEquals(elm.getAttribute("data-foo-bar-baz"), "x")
    patch(p1, vnode2)
    assert(!elm.hasAttribute("data-foo-bar-baz"))
  }

  test("round-trips dashed data attributes through `toVNode`") {
    val elm = dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
    elm.setAttribute("data-foo-bar", "1")
    elm.setAttribute("data-baz", "2")
    val vnode = toVNode(elm)
    vnode match {
      case PatchedVNode.Element(_, data, _, _) =>
        assertEquals(data.dataset, Map("fooBar" -> "1", "baz" -> "2"))
      case other => fail(s"expected element, got $other")
    }
    // patching to a vnode without dataset removes the attributes
    patch(vnode, h("div"))
    assert(!elm.hasAttribute("data-foo-bar"))
    assert(!elm.hasAttribute("data-baz"))
  }

}

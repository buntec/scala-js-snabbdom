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

import org.scalajs.dom

class AttributesSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(Seq(modules.Attributes.module))

  group("attributes") {

    vnode0.test("have their provided values") { vnode0 =>
      val vnode1 = h(
        "div",
        VNodeData.builder
          .withAttrs(
            "href" -> "/foo",
            "minlength" -> 1,
            "selected" -> true,
            "disabled" -> false
          )
          .build
      )
      val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
      assertEquals(elm.getAttribute("href"), "/foo")
      assertEquals(elm.getAttribute("minlength"), "1")
      assertEquals(elm.hasAttribute("selected"), true)
      assertEquals(elm.getAttribute("selected"), "")
      assertEquals(elm.hasAttribute("disabled"), false)
    }

    vnode0.test("can be memoized") { vnode0 =>
      val cachedAttrs = VNodeData.builder
        .withAttrs("href" -> "/foo", "minlength" -> 1, "selected" -> true)
        .build
      val vnode1 = h("div", cachedAttrs)
      val vnode2 = h("div", cachedAttrs)
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
      assertEquals(elm1.getAttribute("href"), "/foo")
      assertEquals(elm1.getAttribute("minlength"), "1")
      assertEquals(elm1.hasAttribute("selected"), true)

      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
      assertEquals(elm2.getAttribute("href"), "/foo")
      assertEquals(elm2.getAttribute("minlength"), "1")
      assertEquals(elm2.hasAttribute("selected"), true)
    }

    vnode0.test("are not omitted when falsy (in JS) values are provided") {
      vnode0 =>
        val vnode1 = h(
          "div",
          VNodeData.builder
            .withAttrs(
              "href" -> None,
              "minlength" -> 0,
              "value" -> "",
              "title" -> "undefined"
            )
            .build
        )
        val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
        assertEquals(elm.hasAttribute("href"), true)
        assertEquals(elm.hasAttribute("minlength"), true)
        assertEquals(elm.hasAttribute("value"), true)
        assertEquals(elm.hasAttribute("title"), true)
    }

    vnode0.test("are set correctly when namespaced") { vnode0 =>
      val vnode1 =
        h("div", VNodeData.builder.withAttrs("xlink:href" -> "#foo").build)
      val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
      assertEquals(
        elm.getAttributeNS("http://www.w3.org/1999/xlink", "href"),
        "#foo"
      )
    }

    test("should not touch class nor id fields") {
      val elm = dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      elm.id = "myId"
      elm.className = "myClass"
      val vnode0 = elm
      val vnode1 = h(
        "div#myId.myClass",
        VNodeData.builder.withAttrs().build,
        Array[VNode]("Hello")
      )
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm1.tagName, "DIV")
      assertEquals(elm1.id, "myId")
      assertEquals(elm1.className, "myClass")
      assertEquals(elm1.textContent, "Hello")
    }

  }

  group("boolean attribute") {

    vnode0.test("is present and empty string if the value is thruthy (in JS)") {
      vnode0 =>
        val vnode1 = h(
          "div",
          VNodeData.builder
            .withAttrs(
              "required" -> true,
              "readonly" -> 1,
              "noresize" -> "truthy"
            )
            .build
        )
        val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm.hasAttribute("required"), true)
        assertEquals(elm.getAttribute("required"), "")
        assertEquals(elm.hasAttribute("readonly"), true)
        assertEquals(elm.getAttribute("readonly"), "1")
        assertEquals(elm.hasAttribute("noresize"), true)
        assertEquals(elm.getAttribute("noresize"), "truthy")
    }

    vnode0.test("is omitted if the value is false") { vnode0 =>
      val vnode1 =
        h("div", VNodeData.builder.withAttrs("required" -> false).build)
      val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm.hasAttribute("required"), false)
      assertEquals(elm.getAttribute("required"), null)
    }

    vnode0.test("is not omitted if the value is falsy") { vnode0 =>
      val vnode1 =
        h(
          "div",
          VNodeData.builder.withAttrs("readonly" -> 0, "noresize" -> "").build
        )
      val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm.hasAttribute("readonly"), true)
      assertEquals(elm.hasAttribute("noresize"), true)
    }

  }

  group("Object.prototype property") {

    // not sure I understand this test
    vnode0.test(
      "is not considered as a boolean attribute and shouldn't be omitted"
    ) { vnode0 =>
      val vnode1 =
        h("div", VNodeData.builder.withAttrs("constructor" -> true).build)
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
      assertEquals(elm1.hasAttribute("constructor"), true)
      assertEquals(elm1.getAttribute("constructor"), "")
      val vnode2 =
        h("div", VNodeData.builder.withAttrs("constructor" -> false).build)
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
      assertEquals(elm2.hasAttribute("constructor"), false)

    }

  }

}

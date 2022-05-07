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

class StyleSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(
    Seq(
      Styles.module
    )
  )

  val hasCssVariables = {
    val featureDiscoveryElm =
      dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
    featureDiscoveryElm.style.setProperty("--foo", "foo")
    featureDiscoveryElm.style.getPropertyValue("--foo") == "foo"
  }

  group("style") {

    vnode0.test("is being styled") { vnode0 =>
      val elm = patch(
        vnode0,
        h("div", VNodeData.builder.withStyle("fontSize" -> "12px").build)
      ).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm.style.fontSize, "12px")
    }

    vnode0.test("can be memoized") { vnode0 =>
      val cachedStyles = VNodeData.builder
        .withStyle("fontSize" -> "14px", "display" -> "inline")
        .build
      val vnode1 = h("i", cachedStyles)
      val vnode2 = h("i", cachedStyles)
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm1.style.fontSize, "14px")
      assertEquals(elm1.style.display, "inline")
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm2.style.fontSize, "14px")
      assertEquals(elm2.style.display, "inline")
    }

    vnode0.test("updates styles") { vnode0 =>
      val vnode1 = h(
        "i",
        VNodeData.builder
          .withStyle("fontSize" -> "14px", "display" -> "inline")
          .build
      )

      val vnode2 = h(
        "i",
        VNodeData.builder
          .withStyle("fontSize" -> "12px", "display" -> "block")
          .build
      )

      val vnode3 = h(
        "i",
        VNodeData.builder
          .withStyle("fontSize" -> "10px", "display" -> "block")
          .build
      )

      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm1.style.fontSize, "14px")
      assertEquals(elm1.style.display, "inline")
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm2.style.fontSize, "12px")
      assertEquals(elm2.style.display, "block")
      val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm3.style.fontSize, "10px")
      assertEquals(elm3.style.display, "block")

    }

    vnode0.test("explicitly removes styles") { vnode0 =>
      val vnode1 =
        h("i", VNodeData.builder.withStyle("fontSize" -> "14px").build)
      val vnode2 =
        h("i", VNodeData.builder.withStyle("fontSize" -> "").build)
      val vnode3 =
        h("i", VNodeData.builder.withStyle("fontSize" -> "10px").build)
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm1.style.fontSize, "14px")
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm2.style.fontSize, "")
      val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm3.style.fontSize, "10px")
    }

    vnode0.test("implicitly removes styles from element") { vnode0 =>
      val vnode1 =
        h("i", VNodeData.builder.withStyle("fontSize" -> "14px").build)
      val vnode2 = h("i", VNodeData.empty)
      val vnode3 =
        h("i", VNodeData.builder.withStyle("fontSize" -> "10px").build)
      val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm1.style.fontSize, "14px")
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm2.style.fontSize, "")
      val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
      assertEquals(elm3.style.fontSize, "10px")
    }

    vnode0.test("updates css variables") { vnode0 =>
      if (hasCssVariables) {

        val vnode1 =
          h("div", VNodeData.builder.withStyle("--myVar" -> "1").build)
        val vnode2 =
          h("div", VNodeData.builder.withStyle("--myVar" -> "2").build)
        val vnode3 =
          h("div", VNodeData.builder.withStyle("--myVar" -> "3").build)

        val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm1.style.getPropertyValue("--myVar"), "1")
        val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm2.style.getPropertyValue("--myVar"), "2")
        val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm3.style.getPropertyValue("--myVar"), "3")

      }
    }

    vnode0.test("explicitly removes css variables") { vnode0 =>
      if (hasCssVariables) {

        val vnode1 =
          h("i", VNodeData.builder.withStyle("--myVar" -> "1").build)
        val vnode2 =
          h("i", VNodeData.builder.withStyle("--myVar" -> "").build)
        val vnode3 =
          h("i", VNodeData.builder.withStyle("--myVar" -> "3").build)

        val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm1.style.getPropertyValue("--myVar"), "1")
        val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm2.style.getPropertyValue("--myVar"), "")
        val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(elm3.style.getPropertyValue("--myVar"), "3")

      }
    }

    vnode0.test("implicitly removes css variables") { vnode0 =>
      if (hasCssVariables) {

        val vnode1 =
          h(
            "div",
            Array(h("i", VNodeData.builder.withStyle("--myVar" -> "1").build))
          )
        val vnode2 =
          h("div", Array(h("i", VNodeData.empty)))
        val vnode3 =
          h(
            "div",
            Array(h("i", VNodeData.builder.withStyle("--myVar" -> "3").build))
          )

        val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(
          elm1.firstChild
            .asInstanceOf[dom.HTMLElement]
            .style
            .getPropertyValue("--myVar"),
          "1"
        )
        val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(
          elm2.firstChild
            .asInstanceOf[dom.HTMLElement]
            .style
            .getPropertyValue("--myVar"),
          ""
        )
        val elm3 = patch(vnode2, vnode3).elm.get.asInstanceOf[dom.HTMLElement]
        assertEquals(
          elm3.firstChild
            .asInstanceOf[dom.HTMLElement]
            .style
            .getPropertyValue("--myVar"),
          "3"
        )

      }
    }

  }

  // TODO: port transition stuff in style module and add tests

}

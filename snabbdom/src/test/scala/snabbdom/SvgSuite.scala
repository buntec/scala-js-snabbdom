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
import snabbdom.modules.Attributes

class SvgSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(Seq(Attributes.module))

  vnode0.test("removes child svg elements") { vnode0 =>
    val a = h("svg", VNodeData(), Array(h("g"), h("g")))
    val b = h("svg", VNodeData(), Array(h("g")))
    val result = patch(patch(vnode0, a), b).elm.asInstanceOf[dom.SVGElement]
    assertEquals(result.childNodes.length, 1)
  }

  vnode0.test("adds correctly xlink namespaced attribute") { vnode0 =>
    val xlinkNS = "http://www.w3.org/1999/xlink"
    val testUrl = "/test"
    val a = h(
      "svg",
      VNodeData(),
      Array(
        h(
          "use",
          VNodeData(attrs = Map("xlink:href" -> testUrl)),
          Array[VNode]()
        )
      )
    )
    val result = patch(vnode0, a).elm.asInstanceOf[dom.SVGElement]
    assertEquals(result.childNodes.length, 1)
    val child = result.childNodes(0).asInstanceOf[dom.SVGUseElement]
    assertEquals(child.getAttribute("xlink:href"), testUrl)
    assertEquals(child.getAttributeNS(xlinkNS, "href"), testUrl)
  }

  vnode0.test("add correctly xml namespaced attribute") { vnode0 =>
    val xmlNS = "http://www.w3.org/XML/1998/namespace"
    val testAttrValue = "und"
    val a = h(
      "svg",
      VNodeData(attrs = Map("xml:lang" -> testAttrValue)),
      Array[VNode]()
    )
    val result = patch(vnode0, a).elm.asInstanceOf[dom.SVGElement]
    assertEquals(result.getAttributeNS(xmlNS, "lang"), testAttrValue)
    assertEquals(result.getAttribute("xml:lang"), testAttrValue)
  }

}

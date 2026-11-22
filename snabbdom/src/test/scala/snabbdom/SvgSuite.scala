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
  val svgNS = "http://www.w3.org/2000/svg"
  val htmlNS = "http://www.w3.org/1999/xhtml"

  vnode0.test("replaces children when their namespace changes") { vnode0 =>
    val first = patch(vnode0, h("div", List(h("a", "HTML"))))
    val html = first.node.firstChild
    val second =
      patch(first, h("div", List(h("a", VNodeData(ns = Some(svgNS)), "SVG"))))
    assert(first.node eq second.node)
    assert(!(second.node.firstChild eq html))
    assertEquals(second.node.firstChild.namespaceURI, svgNS)
    assertEquals(second.node.textContent, "SVG")
  }

  vnode0.test("creates HTML children inside a foreignObject with a selector") {
    vnode0 =>
      val result = patch(
        vnode0,
        h("svg", List(h("foreignObject#box.content", List(h("div", "HTML")))))
      )
      assertEquals(result.node.firstChild.namespaceURI, svgNS)
      assertEquals(result.node.firstChild.firstChild.namespaceURI, htmlNS)
  }

  test("converts existing SVG with case-sensitive tags and HTML children") {
    val svg = dom.document.createElementNS(svgNS, "svg")
    val foreignObject = dom.document.createElementNS(svgNS, "foreignObject")
    foreignObject.setAttribute("class", "content")
    val child = dom.document.createElement("div")
    foreignObject.appendChild(child)
    svg.appendChild(foreignObject)

    val patched = patch(
      toVNode(svg),
      h("svg", List(h("foreignObject.content", List(h("div", "HTML")))))
    )
    assert(patched.node.firstChild eq foreignObject)
    assert(foreignObject.firstChild eq child)
    assertEquals(child.textContent, "HTML")
  }

  vnode0.test("renders and updates thunk children in SVG") { vnode0 =>
    val render = (arg: Any) => h("g", List(h("text", arg.toString)))
    def tree(arg: String) = h("svg", List(thunk("g", "group", render, arg)))
    val first = patch(vnode0, tree("before"))
    val g = first.node.firstChild
    val text = g.firstChild
    assertEquals(g.namespaceURI, svgNS)
    assertEquals(text.namespaceURI, svgNS)

    val second = patch(first, tree("after"))
    assert(second.node.firstChild eq g)
    assert(g.firstChild eq text)
    assertEquals(text.textContent, "after")
  }

  vnode0.test("reuses a thunk until its rendered namespace changes") { vnode0 =>
    var renders = 0
    val render = (arg: Any) => {
      renders += 1
      val namespace = if (arg == "SVG") Some(svgNS) else None
      h("a", VNodeData(ns = namespace), arg.toString)
    }
    def tree(arg: String) = h("div", List(thunk("a", "link", render, arg)))

    val first = patch(vnode0, tree("SVG"))
    val svg = first.node.firstChild
    val second = patch(first, tree("SVG"))
    assert(second.node.firstChild eq svg)
    assertEquals(renders, 1)

    val third = patch(second, tree("HTML"))
    assertEquals(third.node.childNodes.length, 1)
    assertEquals(third.node.firstChild.namespaceURI, htmlNS)
    assertEquals(third.node.textContent, "HTML")
    assertEquals(renders, 2)
  }

  test("mounts a vnode without ns onto an existing SVG element in place") {
    val svg = dom.document.createElementNS(svgNS, "svg")
    val g = dom.document.createElementNS(svgNS, "g")
    g.setAttribute("id", "mount")
    svg.appendChild(g)
    val patched = patch(g, h("g#mount", VNodeData(attrs = Map("x" -> "1"))))
    assert(patched.node eq g)
    assert(svg.firstChild eq g)
    assertEquals(g.getAttribute("x"), "1")
  }

  vnode0.test("removes child svg elements") { vnode0 =>
    val a = h("svg", VNodeData(), List(h("g"), h("g")))
    val b = h("svg", VNodeData(), List(h("g")))
    val result = patch(patch(vnode0, a), b).node.asInstanceOf[dom.SVGElement]
    assertEquals(result.childNodes.length, 1)
  }

  vnode0.test("adds correctly xlink namespaced attribute") { vnode0 =>
    val xlinkNS = "http://www.w3.org/1999/xlink"
    val testUrl = "/test"
    val a = h(
      "svg",
      VNodeData(),
      List(
        h(
          "use",
          VNodeData(attrs = Map("xlink:href" -> testUrl)),
          List[VNode]()
        )
      )
    )
    val result = patch(vnode0, a).node.asInstanceOf[dom.SVGElement]
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
      List[VNode]()
    )
    val result = patch(vnode0, a).node.asInstanceOf[dom.SVGElement]
    assertEquals(result.getAttributeNS(xmlNS, "lang"), testAttrValue)
    assertEquals(result.getAttribute("xml:lang"), testAttrValue)
  }

}

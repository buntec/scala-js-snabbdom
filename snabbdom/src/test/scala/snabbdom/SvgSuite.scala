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
      val namespace = Some(if (arg == "SVG") svgNS else htmlNS)
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

  test("patches a converted SVG group in place from a vnode without ns") {
    val svg = dom.document.createElementNS(svgNS, "svg")
    val g = dom.document.createElementNS(svgNS, "g")
    svg.appendChild(g)
    val patched = patch(toVNode(g), h("g", List(h("circle"))))
    assert(patched.node eq g)
    assert(svg.firstChild eq g)
    assertEquals(g.firstChild.namespaceURI, svgNS)
  }

  test("keeps a mounted SVG group when a later patch names its namespace") {
    val svg = dom.document.createElementNS(svgNS, "svg")
    val g = dom.document.createElementNS(svgNS, "g")
    svg.appendChild(g)
    val first = patch(g, h("g", List(h("circle"))))
    assertEquals(g.firstChild.namespaceURI, svgNS)
    val circle = g.firstChild
    val second = patch(
      first,
      h(
        "g",
        VNodeData(ns = Some(svgNS)),
        List(h("circle", VNodeData(ns = Some(svgNS))))
      )
    )
    assert(second.node eq g)
    assert(g.firstChild eq circle)
  }

  vnode0.test("lets a prepatch hook supply the namespace before replacing") {
    vnode0 =>
      var inits = 0
      def withSvg(vnode: VNode): VNode = vnode match {
        case e: VNode.Element => e.copy(data = e.data.copy(ns = Some(svgNS)))
        case other            => other
      }
      val hook = Hooks(
        init = Some((v: VNode) => { inits += 1; withSvg(v) }),
        prepatch = Some((_: PatchedVNode, v: VNode) => withSvg(v))
      )
      def tree(label: String) =
        h("div", List(h("g", VNodeData(hook = Some(hook)), label)))
      val first = patch(vnode0, tree("a"))
      val g = first.node.firstChild
      assertEquals(g.namespaceURI, svgNS)
      val second = patch(first, tree("b"))
      assert(second.node.firstChild eq g)
      assertEquals(g.textContent, "b")
      assertEquals(inits, 1)
  }

  vnode0.test("keeps an element whose init hook supplied the namespace") {
    vnode0 =>
      var inits = 0
      var destroys = 0
      val hook = Hooks(
        init = Some((v: VNode) => {
          inits += 1
          v match {
            case e: VNode.Element =>
              e.copy(data = e.data.copy(ns = Some(svgNS)))
            case other => other
          }
        }),
        destroy = Some((_: PatchedVNode) => destroys += 1)
      )
      def tree(label: String) =
        h("div", List(h("g", VNodeData(hook = Some(hook)), label)))
      val first = patch(vnode0, tree("a"))
      val g = first.node.firstChild
      assertEquals(g.namespaceURI, svgNS)
      val second = patch(first, tree("b"))
      val third = patch(second, tree("c"))
      assert(second.node.firstChild eq g)
      assert(third.node.firstChild eq g)
      assertEquals(g.textContent, "c")
      assertEquals(inits, 1)
      assertEquals(destroys, 0)
  }

  vnode0.test("runs the init hook when a namespace change replaces a node") {
    vnode0 =>
      var inits = 0
      val hook = Hooks(
        init = Some((v: VNode) => {
          inits += 1
          v match {
            case e: VNode.Element =>
              e.copy(data = e.data.copy(attrs = Map("data-init" -> "yes")))
            case other => other
          }
        }),
        prepatch = Some((_: PatchedVNode, v: VNode) =>
          v match {
            case e: VNode.Element =>
              e.copy(data = e.data.copy(ns = Some(svgNS)))
            case other => other
          }
        )
      )
      def tree = h("div", List(h("g", VNodeData(hook = Some(hook)))))
      val first = patch(vnode0, tree)
      val htmlG = first.node.firstChild
      assertEquals(htmlG.namespaceURI, htmlNS)
      val second = patch(first, tree)
      val svgG = second.node.firstChild.asInstanceOf[dom.Element]
      assert(!(svgG eq htmlG))
      assertEquals(svgG.namespaceURI, svgNS)
      assertEquals(svgG.getAttribute("data-init"), "yes")
      assertEquals(inits, 2)
  }

  test("patches elements in place in a document without an HTML default") {
    val xmlDoc = XmlDomApi.svgDocument()
    val xmlPatch = init(Seq(Attributes.module), Some(XmlDomApi(xmlDoc)))
    def tree(label: String) =
      h("svg", List(h("foreignObject", List(h("div", label)))))
    val root = xmlDoc.documentElement
    val first = xmlPatch(root, tree("a"))
    val div = first.node.firstChild.firstChild
    assertEquals(div.asInstanceOf[dom.Element].namespaceURI, null)
    val second = xmlPatch(first, tree("b"))
    assert(second.node.firstChild.firstChild eq div)
    assertEquals(div.textContent, "b")
  }

  test("treats an explicit HTML namespace like an omitted one") {
    val parent = dom.document.createElement("div")
    val button = dom.document.createElement("button")
    parent.appendChild(button)
    val patched = patch(
      toVNode(button),
      h("button", VNodeData(ns = Some(htmlNS)), "new")
    )
    assert(patched.node eq button)
    assertEquals(button.textContent, "new")
  }

  vnode0.test("replaces a detached SVG root with an HTML element") { vnode0 =>
    val svg = patch(vnode0, h("svg"))
    assertEquals(svg.node.namespaceURI, svgNS)
    val div = patch(svg, h("div", List(h("span", "HTML"))))
    assertEquals(div.node.namespaceURI, htmlNS)
    assertEquals(div.node.firstChild.namespaceURI, htmlNS)
  }

  vnode0.test("replaces a detached SVG root that is named HTML") { vnode0 =>
    val svgA = patch(vnode0, h("a", VNodeData(ns = Some(svgNS)), "SVG"))
    val htmlA = patch(svgA, h("a", VNodeData(ns = Some(htmlNS)), "HTML"))
    assert(!(htmlA.node eq svgA.node))
    assertEquals(htmlA.node.namespaceURI, htmlNS)
  }

  vnode0.test("keeps an SVG element for a vnode without ns") { vnode0 =>
    val first =
      patch(vnode0, h("div", List(h("a", VNodeData(ns = Some(svgNS))))))
    val a = first.node.firstChild
    val second = patch(first, h("div", List(h("a", "text"))))
    assert(second.node.firstChild eq a)
    assertEquals(a.namespaceURI, svgNS)
    assertEquals(a.textContent, "text")
  }

  vnode0.test("lets a thunk's render function name an HTML namespace in SVG") {
    vnode0 =>
      val render = (arg: Any) =>
        h("div", VNodeData(ns = Some(htmlNS)), List(h("span", arg.toString)))
      def tree(arg: String) = h("svg", List(thunk("div", render, arg)))
      val first = patch(vnode0, tree("a"))
      val div = first.node.firstChild
      assertEquals(div.namespaceURI, htmlNS)
      assertEquals(div.firstChild.namespaceURI, htmlNS)
      val second = patch(first, tree("b"))
      assert(second.node.firstChild eq div)
      assertEquals(div.textContent, "b")
  }

  vnode0.test("checks a named namespace only after the prepatch hook") {
    vnode0 =>
      var prepatches = 0
      val hook = Hooks(prepatch = Some((_: PatchedVNode, v: VNode) => {
        prepatches += 1
        v match {
          case e: VNode.Element => e.copy(data = e.data.copy(ns = Some(htmlNS)))
          case other            => other
        }
      }))
      val first = patch(vnode0, h("div", List(h("a", "HTML"))))
      val a = first.node.firstChild
      val second = patch(
        first,
        h("div", List(h("a", VNodeData(ns = Some(svgNS), hook = Some(hook)))))
      )
      assertEquals(prepatches, 1)
      assert(second.node.firstChild eq a)
      assertEquals(a.namespaceURI, htmlNS)
  }

  test("creates an element named HTML in an XML document in HTML") {
    val xmlDoc = XmlDomApi.svgDocument()
    val xmlPatch = init(Seq(Attributes.module), Some(XmlDomApi(xmlDoc)))
    def tree(label: String) =
      h(
        "svg",
        List(
          h(
            "foreignObject",
            List(h("div", VNodeData(ns = Some(htmlNS)), label))
          )
        )
      )
    val first = xmlPatch(xmlDoc.documentElement, tree("a"))
    val div = first.node.firstChild.firstChild
    assertEquals(div.asInstanceOf[dom.Element].namespaceURI, htmlNS)
    val second = xmlPatch(first, tree("b"))
    assert(second.node.firstChild.firstChild eq div)
    assertEquals(div.textContent, "b")
  }

  test("mounts onto an SVG element whose parent is HTML in place") {
    val parent = dom.document.createElement("div")
    val g = dom.document.createElementNS(svgNS, "g")
    parent.appendChild(g)
    val patched = patch(g, h("g", List(h("circle"))))
    assert(patched.node eq g)
    assert(parent.firstChild eq g)
    assertEquals(g.firstChild.namespaceURI, svgNS)
  }

  test("mounts a vnode without ns onto a detached SVG element in place") {
    val g = dom.document.createElementNS(svgNS, "g")
    val patched = patch(g, h("g", List(h("circle"))))
    assert(patched.node eq g)
    assertEquals(g.firstChild.namespaceURI, svgNS)
  }

  test("keeps patching a detached mounted SVG element in place") {
    val g = dom.document.createElementNS(svgNS, "g")
    val first = patch(g, h("g", List(h("circle"))))
    val circle = g.firstChild
    val second = patch(first, h("g", List(h("circle"), h("rect"))))
    assert(second.node eq g)
    assert(g.firstChild eq circle)
    assertEquals(g.lastChild.namespaceURI, svgNS)
  }

  vnode0.test("keeps explicit namespaces of descendants inside svg") { vnode0 =>
    val result = patch(
      vnode0,
      h(
        "svg",
        List(
          h("div", VNodeData(ns = Some(htmlNS)), List(h("span", "HTML"))),
          h("g")
        )
      )
    )
    val div = result.node.firstChild
    assertEquals(div.namespaceURI, htmlNS)
    assertEquals(div.firstChild.namespaceURI, htmlNS)
    assertEquals(result.node.lastChild.namespaceURI, svgNS)
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

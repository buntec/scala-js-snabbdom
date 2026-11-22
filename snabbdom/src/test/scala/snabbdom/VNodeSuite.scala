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

class VNodeSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  private val svgNS = Some("http://www.w3.org/2000/svg")

  private def ns(vnode: VNode): Option[String] = vnode match {
    case VNode.Element(_, data, _) => data.ns
    case _                         => None
  }

  private def children(vnode: VNode): List[VNode] = vnode match {
    case VNode.Element(_, _, children) => children
    case _                             => Nil
  }

  group("hyperscript") {

    test("adds svg namespace for all overloads") {
      val data = VNodeData(attrs = Map("width" -> "1"))
      val child = h("g")
      val all: List[VNode] = List(
        h("svg"),
        h("svg", data),
        h("svg", List(child)),
        h("svg", "text"),
        h("svg", data, "text"),
        h("svg", data, List(child)),
        h("svg", data, child),
        h("svg#id.cls"),
        h("svg.cls", data)
      )
      all.foreach(vnode => assertEquals(ns(vnode), svgNS, vnode))
    }

    test("does not add svg namespace to non-svg elements") {
      val data = VNodeData()
      val all: List[VNode] = List(
        h("svgfoo"),
        h("svgfoo", data),
        h("div", List(h("g"))),
        h("div", "text"),
        h("div", data, "text"),
        h("div", data, List(h("g"))),
        h("div", data, h("g"))
      )
      all.foreach(vnode => assertEquals(ns(vnode), None, vnode))
    }

    test("adds svg namespace to children, except inside foreignObject") {
      val vnode = h(
        "svg",
        VNodeData(),
        List[VNode](
          h("g", List(h("rect"))),
          "text",
          h.comment("comment"),
          h("foreignObject", List(h("div")))
        )
      )
      val List(g, text, comment, fo) = children(vnode): @unchecked
      assertEquals(ns(g), svgNS)
      assertEquals(ns(children(g).head), svgNS)
      assertEquals(text, VNode.text("text"))
      assertEquals(comment, VNode.comment("comment"))
      assertEquals(ns(fo), svgNS)
      assertEquals(ns(children(fo).head), None)
    }

    test("adds svg namespace to a single child") {
      val vnode = h("svg", VNodeData(), h("g", List(h("rect"))))
      val g = children(vnode).head
      assertEquals(ns(g), svgNS)
      assertEquals(ns(children(g).head), svgNS)
    }

  }

  group("VNode") {

    test("`empty` is an empty text node") {
      assertEquals(VNode.empty, VNode.Text(""))
    }

    test("`key` returns the key of elements only") {
      assertEquals(h("div", VNodeData(key = Some("k"))).key, Some("k"))
      assertEquals(h("div").key, None)
      assertEquals(VNode.text("foo").key, None)
      assertEquals(VNode.comment("foo").key, None)
    }

    test("`prettyPrint` renders a readable tree") {
      val vnode = h(
        "div",
        VNodeData(key = Some("root")),
        List[VNode](h("span", "hello"), h.comment("c"))
      )
      assertEquals(
        vnode.prettyPrint,
        """|<div key=root>
           |  <span>
           |    hello
           |  </span>
           |<!--c-->
           |</div>""".stripMargin
      )
    }

    test("`PatchedVNode.key` returns the key of elements only") {
      val elm = dom.document.createElement("div")
      val text = dom.document.createTextNode("t")
      assertEquals(
        PatchedVNode.key(
          PatchedVNode.element("div", VNodeData(key = Some("k")), Nil, elm)
        ),
        Some("k")
      )
      assertEquals(PatchedVNode.key(PatchedVNode.text("t", text)), None)
    }

  }

  group("default modules") {

    vnode0.test("`init.apply` enables all built-in modules") { vnode0 =>
      var clicked = 0
      val patch = init.apply
      val vnode = h(
        "div",
        VNodeData(
          attrs = Map("title" -> "t"),
          classes = Map("active" -> true),
          props = Map("foo" -> "bar"),
          style = Map("color" -> "red"),
          on = Map("click" -> EventHandler((_: dom.Event) => clicked += 1)),
          dataset = Map("fooBar" -> "baz")
        )
      )
      val elm = patch(vnode0, vnode).node.asInstanceOf[dom.HTMLElement]
      elm.click()
      assertEquals(elm.getAttribute("title"), "t")
      assert(elm.classList.contains("active"))
      assertEquals(
        elm.asInstanceOf[scalajs.js.Dictionary[String]]("foo"),
        "bar"
      )
      assertEquals(elm.style.color, "red")
      assertEquals(clicked, 1)
      assertEquals(elm.getAttribute("data-foo-bar"), "baz")
    }

  }

  group("toVNode") {

    test("normalizes whitespace in class names") {
      val elm = dom.document.createElement("div")
      elm.setAttribute("class", "  foo \t bar\nbaz  ")
      toVNode(elm) match {
        case PatchedVNode.Element(sel, _, _, _) =>
          assertEquals(sel, "div.foo.bar.baz")
        case other => fail(s"expected element, got $other")
      }
    }

    test("ignores an empty class attribute") {
      val elm = dom.document.createElement("div")
      elm.setAttribute("class", " ")
      toVNode(elm) match {
        case PatchedVNode.Element(sel, _, _, _) => assertEquals(sel, "div")
        case other => fail(s"expected element, got $other")
      }
    }

    test("converts comment and text nodes") {
      val elm = dom.document.createElement("div")
      elm.appendChild(dom.document.createComment("a comment"))
      elm.appendChild(dom.document.createTextNode("some text"))
      toVNode(elm) match {
        case PatchedVNode.Element(_, _, List(c, t), _) =>
          assertEquals(c.toVNode, VNode.comment("a comment"))
          assertEquals(t.toVNode, VNode.text("some text"))
        case other => fail(s"expected element, got $other")
      }
    }

    test("adds svg namespace to svg elements and their children") {
      val svg = dom.document.createElementNS(svgNS.get, "svg")
      svg.appendChild(dom.document.createElementNS(svgNS.get, "g"))
      toVNode(svg) match {
        case PatchedVNode.Element(_, data, List(g), _) =>
          assertEquals(data.ns, svgNS)
          g match {
            case PatchedVNode.Element(_, data, _, _) =>
              assertEquals(data.ns, svgNS)
            case other => fail(s"expected element, got $other")
          }
        case other => fail(s"expected element, got $other")
      }
    }

    test("throws on unsupported node types") {
      intercept[IllegalArgumentException] {
        toVNode(dom.document.createDocumentFragment())
      }
    }

  }

  group("patching") {

    val patch = init(Seq(modules.Attributes.module))

    vnode0.test("reuses comment nodes among children") { vnode0 =>
      val p1 = patch(vnode0, h("div", List[VNode](h.comment("a"), "x")))
      val comment = p1.node.childNodes(0)
      patch(p1, h("div", List[VNode](h.comment("b"), "x")))
      assert(p1.node.childNodes(0) eq comment)
      assertEquals(comment.textContent, "b")
    }

    vnode0.test("updates the content of a root comment") { vnode0 =>
      val p1 = patch(vnode0, h.comment("first"))
      val node = p1.node
      val p2 = patch(p1, h.comment("second"))
      assert(p2.node eq node)
      assertEquals(node.textContent, "second")
      assertEquals(p2.toVNode, VNode.comment("second"))
    }

    vnode0.test("removes comments alongside retained children") { vnode0 =>
      val p1 = patch(vnode0, h("div", List(h.comment("remove"), h("span"))))
      val span = p1.node.lastChild
      val p2 = patch(p1, h("div", List(h("span"))))
      assertEquals(p2.node.childNodes.length, 1)
      assert(p2.node.firstChild eq span)
    }

    vnode0.test("removes comments when clearing children") { vnode0 =>
      val p1 = patch(vnode0, h("div", List(h.comment("remove"))))
      val p2 = patch(p1, h("div"))
      assertEquals(p2.node.childNodes.length, 0)
    }

    test("removes a root comment when replacing it") {
      val parent = dom.document.createElement("div")
      val comment = dom.document.createComment("remove")
      parent.appendChild(comment)
      val p = patch(toVNode(comment), h("span"))
      assertEquals(parent.childNodes.length, 1)
      assert(parent.firstChild eq p.node)
    }

    vnode0.test("reuses a mount element with whitespace in its classes") {
      vnode0 =>
        vnode0.setAttribute("class", "  one\ttwo\n")
        assert(patch(vnode0, h("div.one.two")).node eq vnode0)
    }

    vnode0.test("inserts an unkeyed node before keyed siblings") { vnode0 =>
      def span(key: String) = h("span", VNodeData(key = Some(key)), key)
      val p1 = patch(vnode0, h("div", List(span("a"), span("b"))))
      val elm = p1.node
      val a = elm.childNodes(0)
      val b = elm.childNodes(1)
      val p2 = patch(p1, h("div", List(h("i", "new"), span("a"), span("b"))))
      assertEquals(elm.childNodes.length, 3)
      assertEquals(elm.childNodes(0).nodeName, "I")
      assert(elm.childNodes(1) eq a)
      assert(elm.childNodes(2) eq b)
      assertEquals(
        p2.toVNode,
        h("div", List(h("i", "new"), span("a"), span("b")))
      )
    }

    vnode0.test("removes every unmatched child with a repeated key") { vnode0 =>
      val data = VNodeData(key = Some("duplicate"))
      val p1 = patch(
        vnode0,
        h("div", List(h("span", data, "a"), h("span", data, "b")))
      )
      val p2 = patch(p1, h("div", List(h("i", "new"))))
      assertEquals(p2.node.childNodes.length, 1)
      assertEquals(p2.node.textContent, "new")
    }

  }

}

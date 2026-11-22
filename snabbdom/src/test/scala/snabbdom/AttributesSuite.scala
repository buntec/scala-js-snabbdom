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
        VNodeData(attrs =
          Map(
            "href" -> "/foo",
            "minlength" -> "1",
            "selected" -> true,
            "disabled" -> false
          )
        )
      )
      val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.Element]
      assertEquals(elm.getAttribute("href"), "/foo")
      assertEquals(elm.getAttribute("minlength"), "1")
      assertEquals(elm.hasAttribute("selected"), true)
      assertEquals(elm.getAttribute("selected"), "")
      assertEquals(elm.hasAttribute("disabled"), false)
    }

    vnode0.test("can be memoized") { vnode0 =>
      val cachedAttrs = VNodeData(
        attrs = Map("href" -> "/foo", "minlength" -> "1", "selected" -> true)
      )
      val vnode1 = h("div", cachedAttrs)
      val vnode2 = h("div", cachedAttrs)
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.node.asInstanceOf[dom.Element]
      assertEquals(elm1.getAttribute("href"), "/foo")
      assertEquals(elm1.getAttribute("minlength"), "1")
      assertEquals(elm1.hasAttribute("selected"), true)

      val elm2 = patch(vnode1p, vnode2).node.asInstanceOf[dom.Element]
      assertEquals(elm2.getAttribute("href"), "/foo")
      assertEquals(elm2.getAttribute("minlength"), "1")
      assertEquals(elm2.hasAttribute("selected"), true)
    }

    vnode0.test("are not omitted when falsy (in JS) values are provided") {
      vnode0 =>
        val vnode1 = h(
          "div",
          VNodeData(attrs =
            Map(
              "href" -> "None",
              "minlength" -> "0",
              "value" -> "",
              "title" -> "undefined"
            )
          )
        )
        val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.Element]
        assertEquals(elm.hasAttribute("href"), true)
        assertEquals(elm.hasAttribute("minlength"), true)
        assertEquals(elm.hasAttribute("value"), true)
        assertEquals(elm.hasAttribute("title"), true)
    }

    vnode0.test("are set correctly when namespaced") { vnode0 =>
      val vnode1 =
        h("div", VNodeData(attrs = Map("xlink:href" -> "#foo")))
      val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.Element]
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
        VNodeData(),
        List[VNode]("Hello")
      )
      val elm1 = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLElement]
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
          VNodeData(attrs =
            Map(
              "required" -> true,
              "readonly" -> "1",
              "noresize" -> "truthy"
            )
          )
        )
        val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLElement]
        assertEquals(elm.hasAttribute("required"), true)
        assertEquals(elm.getAttribute("required"), "")
        assertEquals(elm.hasAttribute("readonly"), true)
        assertEquals(elm.getAttribute("readonly"), "1")
        assertEquals(elm.hasAttribute("noresize"), true)
        assertEquals(elm.getAttribute("noresize"), "truthy")
    }

    vnode0.test("is omitted if the value is false") { vnode0 =>
      val vnode1 = h("div", VNodeData(attrs = Map("required" -> false)))
      val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLElement]
      assertEquals(elm.hasAttribute("required"), false)
      assertEquals(elm.getAttribute("required"), null)
    }

    vnode0.test("is not omitted if the value is falsy") { vnode0 =>
      val vnode1 =
        h(
          "div",
          VNodeData(attrs = Map("readonly" -> "0", "noresize" -> ""))
        )
      val elm = patch(vnode0, vnode1).node.asInstanceOf[dom.HTMLElement]
      assertEquals(elm.hasAttribute("readonly"), true)
      assertEquals(elm.hasAttribute("noresize"), true)
    }

  }

  group("Object.prototype property") {

    // not sure I understand this test
    vnode0.test(
      "is not considered as a boolean attribute and shouldn't be omitted"
    ) { vnode0 =>
      val vnode1 = h("div", VNodeData(attrs = Map("constructor" -> true)))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.node.asInstanceOf[dom.Element]
      assertEquals(elm1.hasAttribute("constructor"), true)
      assertEquals(elm1.getAttribute("constructor"), "")
      val vnode2 =
        h("div", VNodeData(attrs = Map("constructor" -> false)))
      val elm2 = patch(vnode1p, vnode2).node.asInstanceOf[dom.Element]
      assertEquals(elm2.hasAttribute("constructor"), false)

    }

  }

  group("updating attributes") {

    vnode0.test("changes, adds and removes attributes") { vnode0 =>
      val vnode1 =
        h("div", VNodeData(attrs = Map("href" -> "/foo", "title" -> "t")))
      val vnode2 =
        h("div", VNodeData(attrs = Map("href" -> "/bar", "lang" -> "en")))
      val p1 = patch(vnode0, vnode1)
      val elm = p1.node.asInstanceOf[dom.Element]
      patch(p1, vnode2)
      assertEquals(elm.getAttribute("href"), "/bar")
      assertEquals(elm.getAttribute("lang"), "en")
      assert(!elm.hasAttribute("title"))
    }

    vnode0.test("toggles boolean attributes") { vnode0 =>
      val on = h("input", VNodeData(attrs = Map("disabled" -> true)))
      val off = h("input", VNodeData(attrs = Map("disabled" -> false)))
      val p1 = patch(vnode0, on)
      val elm = p1.node.asInstanceOf[dom.Element]
      assertEquals(elm.getAttribute("disabled"), "")
      val p2 = patch(p1, off)
      assert(!elm.hasAttribute("disabled"))
      patch(p2, on)
      assertEquals(elm.getAttribute("disabled"), "")
    }

    vnode0.test("updates namespaced attributes") { vnode0 =>
      val xlinkNS = "http://www.w3.org/1999/xlink"
      val xmlNS = "http://www.w3.org/XML/1998/namespace"
      def vnode(href: String, lang: String) =
        h(
          "svg",
          VNodeData(),
          List(
            h(
              "use",
              VNodeData(attrs =
                Map(
                  "xlink:href" -> href,
                  "xml:lang" -> lang,
                  "xyz" -> href
                )
              )
            )
          )
        )
      val p1 = patch(vnode0, vnode("#a", "en"))
      val use = p1.node.childNodes(0).asInstanceOf[dom.Element]
      patch(p1, vnode("#b", "de"))
      assertEquals(use.getAttributeNS(xlinkNS, "href"), "#b")
      assertEquals(use.getAttributeNS(xmlNS, "lang"), "de")
      assertEquals(use.getAttribute("xyz"), "#b")
    }

    vnode0.test("sets and updates xmlns namespace declarations") { vnode0 =>
      val xmlnsNS = "http://www.w3.org/2000/xmlns/"
      val xlinkNS = "http://www.w3.org/1999/xlink"
      def vnode(ns: String) =
        h("svg", VNodeData(attrs = Map("xmlns:xlink" -> AttrValue(ns))))
      val p1 = patch(vnode0, vnode(xlinkNS))
      val svg = p1.node.asInstanceOf[dom.Element]
      assertEquals(svg.getAttribute("xmlns:xlink"), xlinkNS)
      assertEquals(svg.getAttributeNS(xmlnsNS, "xlink"), xlinkNS)
      patch(p1, vnode("urn:other"))
      assertEquals(svg.getAttributeNS(xmlnsNS, "xlink"), "urn:other")
    }

    test("replaces prefixed attributes of parsed SVG instead of duplicating") {
      val xmlnsNS = "http://www.w3.org/2000/xmlns/"
      val xlinkNS = "http://www.w3.org/1999/xlink"
      val container = dom.document.createElement("div")
      container.innerHTML =
        """<svg xmlns:foo="urn:old" xlink:foo="old" xml:foo="old"></svg>"""
      val svg = container.firstChild.asInstanceOf[dom.Element]
      def named(key: String) =
        (0 until svg.attributes.length)
          .map(svg.attributes.item(_))
          .filter(_.name == key)
      val keys = List("xmlns:foo", "xlink:foo", "xml:foo")
      // The parser leaves these in no namespace.
      keys.foreach(key =>
        assertEquals(named(key).map(_.namespaceURI).toList, List[String](null))
      )

      val p1 = patch(
        toVNode(svg),
        h(
          "svg",
          VNodeData(attrs =
            Map[String, AttrValue](
              "xmlns:foo" -> "urn:new",
              "xlink:foo" -> "new",
              "xml:foo" -> "new"
            )
          )
        )
      )
      assert(p1.node eq svg)
      keys.foreach(key => assertEquals(named(key).size, 1, key))
      assertEquals(svg.getAttributeNS(xmlnsNS, "foo"), "urn:new")
      assertEquals(svg.getAttributeNS(xlinkNS, "foo"), "new")
      assertEquals(svg.getAttribute("xmlns:foo"), "urn:new")

      patch(p1, h("svg"))
      keys.foreach(key => assertEquals(named(key).size, 0, key))
    }

    vnode0.test("sets a default xmlns declaration in the XMLNS namespace") {
      vnode0 =>
        val xmlnsNS = "http://www.w3.org/2000/xmlns/"
        val svgNS = "http://www.w3.org/2000/svg"
        val p1 =
          patch(vnode0, h("svg", VNodeData(attrs = Map("xmlns" -> svgNS))))
        val svg = p1.node.asInstanceOf[dom.Element]
        assertEquals(svg.getAttributeNS(xmlnsNS, "xmlns"), svgNS)
        patch(p1, h("svg"))
        assert(!svg.hasAttribute("xmlns"))
    }

    vnode0.test("sets non-namespaced attributes starting with `x`") { vnode0 =>
      val elm = patch(
        vnode0,
        h("div", VNodeData(attrs = Map("xyz" -> "1", "xmlfoo" -> "2")))
      ).node.asInstanceOf[dom.Element]
      assertEquals(elm.getAttribute("xyz"), "1")
      assertEquals(elm.getAttribute("xmlfoo"), "2")
    }

  }

}

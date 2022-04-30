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
import scalajs.js

class CreatedElementSuite extends munit.FunSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(
    Seq(
      Classes.module,
      Props.module,
      EventListeners.module
    )
  )

  vnode0.test("created element has tag") { vnode0 =>
    val elm = patch(vnode0, h("div")).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm.tagName, "DIV")
  }

  vnode0.test("created element has different tag and id") { vnode0 =>
    val elm = dom.document.createElement("div")
    vnode0.appendChild(elm)
    val vnode1 = h("span#id")
    val patched = patch(elm, vnode1).elm.get.asInstanceOf[dom.HTMLSpanElement]
    assertEquals(patched.tagName, "SPAN")
    assertEquals(patched.id, "id")
  }

  vnode0.test("created element has id") { vnode0 =>
    val elm = patch(vnode0, h("div", Array(h("div#unique")))).elm.get
    assertEquals(elm.firstChild.asInstanceOf[dom.Element].id, "unique")
  }

  vnode0.test("created element has correct namespace") { vnode0 =>
    val SVGNamespace = "http://www.w3.org/2000/svg";
    val XHTMLNamespace = "http://www.w3.org/1999/xhtml";

    val data = VNodeData.builder.withNs(SVGNamespace).build

    val elm1 = patch(vnode0, h("div", Array(h("div", data)))).elm.get
    assertEquals(elm1.firstChild.namespaceURI, SVGNamespace)

    // verify that svg tag automatically gets svg namespace
    val elm2 = patch(
      vnode0,
      h(
        "svg",
        Array(
          h(
            "foreignObject",
            Array(h("div", Array[VNode]("I am HTML embedded in SVG")))
          )
        )
      )
    ).elm.get

    assertEquals(elm2.namespaceURI, SVGNamespace)
    assertEquals(elm2.firstChild.namespaceURI, SVGNamespace)
    assertEquals(elm2.firstChild.firstChild.namespaceURI, XHTMLNamespace)

    // verify that svg tag with extra selectors gets svg namespace
    val elm3 = patch(vnode0, h("svg#some-id")).elm.get
    assertEquals(elm3.namespaceURI, SVGNamespace);

    // verify that non-svg tag beginning with 'svg' does NOT get namespace
    val elm4 = patch(vnode0, h("svg-custom-el")).elm.get
    assertNotEquals(elm4.namespaceURI, SVGNamespace)

  }

  vnode0.test("created element receives classes in selector") { vnode0 =>
    val elm = patch(vnode0, h("div", Array(h("i.am.a.class")))).elm.get
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("am"))
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("class"))
  }

  vnode0.test("created element receives classes in class property") { vnode0 =>
    val data = VNodeData.builder
      .withClasses(("am", true), ("a", true), ("class", true), ("not", false))
      .build
    val elm = patch(vnode0, h("i", data)).elm.get
    assert(elm.asInstanceOf[dom.Element].classList.contains("am"))
    assert(elm.asInstanceOf[dom.Element].classList.contains("a"))
    assert(elm.asInstanceOf[dom.Element].classList.contains("class"))
    assert(!elm.asInstanceOf[dom.Element].classList.contains("not"))
  }

  vnode0.test("created element receives classes in selector when namespaced") {
    vnode0 =>
      val elm = patch(vnode0, h("svg", Array(h("g.am.a.class.too")))).elm.get
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("am"))
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
      assert(
        elm.firstChild.asInstanceOf[dom.Element].classList.contains("class")
      )
  }

  vnode0.test(
    "created element receives classes in class property when namespaced"
  ) { vnode0 =>
    val data = VNodeData.builder
      .withClasses(("am", true), ("a", true), ("class", true), ("not", false))
      .build
    val elm = patch(vnode0, h("svg", Array(h("g", data)))).elm.get
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("am"))
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("class"))
    assert(!elm.firstChild.asInstanceOf[dom.Element].classList.contains("not"))
  }

  vnode0.test(
    "created element handles classes from both selector and property"
  ) { vnode0 =>
    val data = VNodeData.builder.withClasses("classes" -> true).build
    val elm = patch(vnode0, h("div", Array(h("i.has", data)))).elm.get
    assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("has"))
    assert(
      elm.firstChild.asInstanceOf[dom.Element].classList.contains("classes")
    )
  }

  vnode0.test("created element can create elements with text content") {
    vnode0 =>
      val elm = patch(vnode0, h("div", Array[VNode]("I am a string"))).elm.get
      assertEquals(elm.asInstanceOf[dom.Element].innerHTML, "I am a string")
  }

  vnode0.test(
    "created element can create elements with spand and text content"
  ) { vnode0 =>
    val elm =
      patch(vnode0, h("a", Array[VNode](h("span"), "I am a string"))).elm.get
    assertEquals(elm.childNodes(0).asInstanceOf[dom.Element].tagName, "SPAN")
    assertEquals(
      elm.childNodes(1).asInstanceOf[dom.Element].textContent,
      "I am a string"
    )
  }

  vnode0.test(
    "created element can create vnode with array String obj content"
  ) { vnode0 =>
    val elm = patch(vnode0, h("a", Array[VNode]("b", "c"))).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].innerHTML, "bc")
  }

  vnode0.test("created element can create elements with props") { vnode0 =>
    val data = VNodeData.builder.withProps("src" -> "http://localhost/").build
    val elm = patch(vnode0, h("a", data)).elm.get
    assertEquals(
      elm.asInstanceOf[js.Dictionary[String]]("src"),
      "http://localhost/"
    )
  }

  vnode0.test(
    "created element can create and element created inside an iframe"
  ) { _ =>
    // TODO
  }

  test("created element is a patch of the root element".only) {
    val elmWithIdAndClass = dom.document.createElement("div")
    elmWithIdAndClass.id = "id"
    elmWithIdAndClass.asInstanceOf[dom.HTMLElement].className = "class"
    val vnode1 = h("div#id.class", Array(h("span", "Hi")));
    val elm = patch(elmWithIdAndClass, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm, elmWithIdAndClass)
    assertEquals(elm.tagName, "DIV")
    assertEquals(elm.id, "id")
    assertEquals(elm.asInstanceOf[dom.HTMLElement].className, "class")
  }

}

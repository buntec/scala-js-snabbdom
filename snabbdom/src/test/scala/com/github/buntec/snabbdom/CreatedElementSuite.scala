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

  def spanNum(s: String) = h("span", VNodeData.empty, s)
  def spanNum(i: Int) =
    h("span", VNodeData.builder.withKey(i.toString).build, i.toString)

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
      EventListeners.module,
      Styles.module
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
      .withClasses("am" -> true, "a" -> true, "class" -> true, "not" -> false)
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
      .withClasses("am" -> true, "a" -> true, "class" -> true, "not" -> false)
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

  test("created element is a patch of the root element") {
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

  vnode0.test("created element can create comments") { vnode0 =>
    val elm = patch(vnode0, h("!", "test")).elm.get
    assertEquals(elm.nodeType, dom.Node.COMMENT_NODE)
    assertEquals(elm.textContent, "test")
  }

  // created document fragment

  vnode0.test("created document fragment is an instance of DocumentFragment") {
    vnode0 =>
      val vnode1 =
        fragment(
          Array[VNode]("I am", h("span", Array[VNode](" a", " fragment")))
        )
      val elm = patch(vnode0, vnode1).elm.get
      assertEquals(elm.nodeType, dom.Node.DOCUMENT_FRAGMENT_NODE)
      assertEquals(elm.textContent, "I am a fragment")
  }

  // patching an element
  vnode0.test("patching an element changes the elements classes") { vnode0 =>
    val vnode1 = h(
      "i",
      VNodeData.builder
        .withClasses("i" -> true, "am" -> true, "horse" -> true)
        .build
    )
    val vnode2 = h(
      "i",
      VNodeData.builder
        .withClasses("i" -> true, "am" -> true, "horse" -> false)
        .build
    )
    patch(vnode0, vnode1)
    val elm = patch(vnode1, vnode2).elm.get
    assert(elm.asInstanceOf[dom.Element].classList.contains("i"))
    assert(elm.asInstanceOf[dom.Element].classList.contains("am"))
    assert(!elm.asInstanceOf[dom.Element].classList.contains("horse"))
  }

  vnode0.test("patching an element changes classes in selector") { _ =>
    // TODO: looks exactly like the the one above in the original???
  }

  vnode0.test("patching an element preserves memoized classes") { vnode0 =>
    val cachedClasses =
      VNodeData.builder
        .withClasses("i" -> true, "am" -> true, "horse" -> false)
        .build
    val vnode1 = h("i", cachedClasses)
    val vnode2 = h("i", cachedClasses)
    val elm = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assert(elm.classList.contains("i"))
    assert(elm.classList.contains("am"))
    assert(!elm.classList.contains("horse"))
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assert(elm2.classList.contains("i"))
    assert(elm2.classList.contains("am"))
    assert(!elm2.classList.contains("horse"))
  }

  vnode0.test("patching an element removes missing classes") { vnode0 =>
    val vnode1 = h(
      "i",
      VNodeData.builder
        .withClasses("i" -> true, "am" -> true, "horse" -> true)
        .build
    )
    val vnode2 = h(
      "i",
      VNodeData.builder
        .withClasses("i" -> true, "am" -> true)
        .build
    )
    patch(vnode0, vnode1)
    val elm = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assert(elm.classList.contains("i"))
    assert(elm.classList.contains("am"))
    assert(!elm.classList.contains("horse"))
  }

  vnode0.test("patching an element changes an elements props") { vnode0 =>
    val vnode1 =
      h("a", VNodeData.builder.withProps("src" -> "http://other/").build)
    val vnode2 =
      h("a", VNodeData.builder.withProps("src" -> "http://localhost/").build)
    patch(vnode0, vnode1)
    val elm = patch(vnode1, vnode2).elm.get.asInstanceOf[js.Dictionary[String]]
    assertEquals(elm("src"), "http://localhost/")
  }

  vnode0.test("patching an element preserves memoized props") { vnode0 =>
    val cachedProps =
      VNodeData.builder.withProps("src" -> "http://other/").build
    val vnode1 = h("a", cachedProps)
    val vnode2 = h("a", cachedProps)
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(
      elm.asInstanceOf[js.Dictionary[String]]("src"),
      "http://other/"
    )
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[js.Dictionary[String]]("src"),
      "http://other/"
    )
  }

  vnode0.test("can set prop value to empty string") { vnode0 =>
    val vnode1 =
      h("p", VNodeData.builder.withProps("textContent" -> "foo").build)
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.HTMLParagraphElement].textContent, "foo")
    val vnode2 = h("p", VNodeData.builder.withProps("textContent" -> "").build)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.HTMLParagraphElement].textContent, "")

  }

  // TODO: This appears to be a bug in the orignal: https://github.com/snabbdom/snabbdom/pull/1019
  vnode0.test("patching an element removes custom props".ignore) { vnode0 =>
    val vnode1 =
      h("a", VNodeData.builder.withProps("src" -> "http://other/").build)
    val vnode2 = h("a")
    patch(vnode0, vnode1)
    val elm = patch(vnode1, vnode2).elm.get
    assert(!elm.asInstanceOf[js.Dictionary[String]].contains("src"))
  }

  vnode0.test("patching an element cannot remove native props") { vnode0 =>
    val vnode1 =
      h("a", VNodeData.builder.withProps("href" -> "http://example.com/").build)
    val vnode2 = h("a")
    val elm1 = patch(vnode0, vnode1).elm.get
    assert(elm1.isInstanceOf[dom.HTMLAnchorElement])
    assertEquals(
      elm1.asInstanceOf[dom.HTMLAnchorElement].href,
      "http://example.com/"
    )
    val elm2 = patch(vnode1, vnode2).elm.get
    assert(elm2.isInstanceOf[dom.HTMLAnchorElement])
    assertEquals(
      elm2.asInstanceOf[dom.HTMLAnchorElement].href,
      "http://example.com/"
    )

  }

  vnode0.test("patching an element does not delete custom props") { vnode0 =>
    val vnode1 = h("p", VNodeData.builder.withProps("a" -> "foo").build)
    val vnode2 = h("p")
    val elm = patch(vnode0, vnode1).elm.get
    assert(elm.isInstanceOf[dom.HTMLParagraphElement])
    assertEquals(elm.asInstanceOf[js.Dictionary[String]]("a"), "foo")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[js.Dictionary[String]]("a"), "foo")
  }

  vnode0.test("addition of elements appends elements") { vnode0 =>
    val vnode1 = h("span", Array("1").map(spanNum))
    val vnode2 = h("span", Array("1", "2", "3").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 1)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
    assertEquals(elm2.asInstanceOf[dom.Element].children(1).innerHTML, "2")
    assertEquals(elm2.asInstanceOf[dom.Element].children(2).innerHTML, "3")
  }

  vnode0.test("prepends elements") { vnode0 =>
    val vnode1 = h("span", Array("4", "5").map(spanNum))
    val vnode2 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 2)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3", "4", "5")
    )
  }

  vnode0.test("add elements in the middle") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "4", "5").map(spanNum))
    val vnode2 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3", "4", "5")
    )
  }

  vnode0.test("add elements at begin and end") { vnode0 =>
    val vnode1 = h("span", Array("2", "3", "4").map(spanNum))
    val vnode2 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3", "4", "5")
    )
  }

  vnode0.test("adds children to parent with no children") { vnode0 =>
    val vnode1 = h("span", VNodeData.builder.withKey("span").build)
    val vnode2 = h(
      "span",
      VNodeData.builder.withKey("span").build,
      Array("1", "2", "3").map(spanNum)
    )
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 0)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3")
    )
  }

  vnode0.test("removes all children to parent") { vnode0 =>
    val vnode1 = h(
      "span",
      VNodeData.builder.withKey("span").build,
      Array("1", "2", "3").map(spanNum)
    )
    val vnode2 = h("span", VNodeData.builder.withKey("span").build)
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(
      elm.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3")
    )
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 0)
  }

  vnode0.test("update one child with same key but different sel") { vnode0 =>
    val data = VNodeData.builder.withKey("span").build
    val data2 = VNodeData.builder.withKey("2").build
    val vnode1 = h("span", data, Array("1", "2", "3").map(spanNum))
    val vnode2 =
      h("span", data, Array(spanNum("1"), h("i", data2, "2"), spanNum("3")))

    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(
      elm.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3")
    )

    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3")
    )

    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
    assertEquals(elm2.asInstanceOf[dom.Element].children(1).tagName, "I")

  }

  vnode0.test("removal of elements removes elements from the beginning") {
    vnode0 =>
      val vnode1 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", Array("3", "4", "5").map(spanNum))
      val elm = patch(vnode0, vnode1).elm.get
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val elm2 = patch(vnode1, vnode2).elm.get
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("3", "4", "5")
      )
  }

  vnode0.test("removal of elements removes elements from the end") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
    val vnode2 = h("span", Array("1", "2", "3").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "2", "3")
    )
  }

  vnode0.test("removal of elements removes elements from the middle") {
    vnode0 =>
      val vnode1 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", Array("1", "2", "4", "5").map(spanNum))
      val elm = patch(vnode0, vnode1).elm.get
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val elm2 = patch(vnode1, vnode2).elm.get
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "4", "5")
      )
  }

  vnode0.test("element reordering moves element forward") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3", "4").map(spanNum))
    val vnode2 = h("span", Array("2", "3", "1", "4").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("2", "3", "1", "4")
    )
  }

  vnode0.test("element reordering moves element to end") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3").map(spanNum))
    val vnode2 = h("span", Array("2", "3", "1").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("2", "3", "1")
    )
  }

  vnode0.test("element reordering moves element backwards") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3", "4").map(spanNum))
    val vnode2 = h("span", Array("1", "4", "2", "3").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("1", "4", "2", "3")
    )
  }

  vnode0.test("element reordering swaps first and last") { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3", "4").map(spanNum))
    val vnode2 = h("span", Array("4", "2", "3", "1").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("4", "2", "3", "1")
    )
  }

  vnode0.test(
    "combinations of additions, removals and reorderings - move to left and replace"
  ) { vnode0 =>
    val vnode1 = h("span", Array("1", "2", "3", "4", "5").map(spanNum))
    val vnode2 = h("span", Array("4", "1", "2", "3", "6").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 5)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("4", "1", "2", "3", "6")
    )
  }

  vnode0.test(
    "combinations of additions, removals and reorderings - moves to left and leaves hole"
  ) { vnode0 =>
    val vnode1 = h("span", Array("1", "4", "5").map(spanNum))
    val vnode2 = h("span", Array("4", "6").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 2)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("4", "6")
    )
  }

  vnode0.test(
    "combinations of additions, removals and reorderings - handles moved and set to undefined element ending at the end"
  ) { vnode0 =>
    val vnode1 = h("span", Array("2", "4", "5").map(spanNum))
    val vnode2 = h("span", Array("4", "5", "3").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("4", "5", "3")
    )
  }

  vnode0.test(
    "combinations of additions, removals and reorderings - moves a key in non-keyed nodes with a size up"
  ) { vnode0 =>
    val vnode1 =
      h("span", Array(spanNum(1)) ++ Array("a", "b", "c").map(spanNum))
    val vnode2 = h(
      "span",
      Array("d", "a", "b", "c").map(spanNum) ++ Array(spanNum(1)) ++ Array(
        spanNum("e")
      )
    )
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
    assertEquals(elm.textContent, "1abc")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 6)
    assertEquals(elm2.textContent, "dabc1e")
  }

  // accepts symbol as key - doesn't apply to Scala.js

  vnode0.test(
    "combinations of additions, removals and reorderings - reverses elements"
  ) { vnode0 =>
    val vnode1 =
      h("span", Array("1", "2", "3", "4", "5", "6", "7", "8").map(spanNum))
    val vnode2 =
      h("span", Array("8", "7", "6", "5", "4", "3", "2", "1").map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 8)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 8)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("8", "7", "6", "5", "4", "3", "2", "1")
    )
  }

  vnode0.test(
    "combinations of additions, removals and reorderings - something"
  ) { vnode0 =>
    val vnode1 =
      h("span", Array(0, 1, 2, 3, 4, 5).map(spanNum))
    val vnode2 =
      h("span", Array(4, 3, 2, 1, 5, 0).map(spanNum))
    val elm = patch(vnode0, vnode1).elm.get
    assertEquals(elm.asInstanceOf[dom.Element].children.length, 6)
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.asInstanceOf[dom.Element].children.length, 6)
    assertEquals(
      elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
      List("4", "3", "2", "1", "5", "0")
    )
  }

  def spanNumWithOpacity(n: Int, o: String) = {
    h(
      "span",
      VNodeData.builder.withKey(n.toString).withStyle("opacity" -> o).build,
      n.toString
    )
  }

  // it is odd that the orginal passes without using the styles module...
  test(
    "combinations of additions, removals and reorderings - handles random shuffles"
  ) {
    val elms = 14
    val samples = 5
    val arr = Array.tabulate(elms)(i => i)
    val rng = new scala.util.Random

    (0 until samples).foreach { _ =>
      val vnode1 = h("span", arr.map(spanNumWithOpacity(_, "1")))
      val shufArr = rng.shuffle(arr)
      val elm = dom.document.createElement("div")
      val elm1 = patch(elm, vnode1).elm.get.asInstanceOf[dom.HTMLSpanElement]
      assertEquals(
        elm1.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        arr.map(_.toString).toList
      )
      val opacities = Array.tabulate(elms)(_ => f"${rng.nextDouble()}%.5f")
      val vnode2 =
        h("span", arr.map(n => spanNumWithOpacity(shufArr(n), opacities(n))))
      val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.HTMLSpanElement]
      (0 until elms).foreach { i =>
        assertEquals(elm2.children(i).innerHTML, shufArr(i).toString)
        val opacity =
          elm2.children(i).asInstanceOf[dom.HTMLSpanElement].style.opacity
        assertEquals(opacity.toDouble, opacities(i).toDouble)
      }
    }
  }

  vnode0.test("updated children without keys - appends elements") { vnode0 =>
    val vnode1 = h("div", Array(h("span", "Hello")))
    val vnode2 = h("div", Array(h("span", "Hello"), h("span", "World")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm1.children.toSeq.map(_.innerHTML), List("Hello"))
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
  }

  vnode0.test("updated children without keys - handles unmoved text nodes") {
    vnode0 =>
      val vnode1 = h("div", Array[VNode]("Text", h("span", "Span")))
      val vnode2 = h("div", Array[VNode]("Text", h("span", "Span")))
      val elm1 = patch(vnode0, vnode1).elm.get
      assertEquals(elm1.childNodes(0).textContent, "Text")
      val elm2 = patch(vnode1, vnode2).elm.get
      assertEquals(elm2.childNodes(0).textContent, "Text")
  }

  vnode0.test(
    "updated children without keys - handles changing text children"
  ) { vnode0 =>
    val vnode1 = h("div", Array[VNode]("Text", h("span", "Span")))
    val vnode2 = h("div", Array[VNode]("Text2", h("span", "Span")))
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(elm1.childNodes(0).textContent, "Text")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.childNodes(0).textContent, "Text2")
  }

  vnode0.test(
    "updated children without keys - handles unmoved comment nodes"
  ) { vnode0 =>
    val vnode1 = h("div", Array[VNode](h("!", "Text"), h("span", "Span")))
    val vnode2 = h("div", Array[VNode](h("!", "Text"), h("span", "Span")))
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(elm1.childNodes(0).textContent, "Text")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.childNodes(0).textContent, "Text")
  }

  vnode0.test(
    "updated children without keys - handles changing comment text"
  ) { vnode0 =>
    val vnode1 = h("div", Array[VNode](h("!", "Text"), h("span", "Span")))
    val vnode2 = h("div", Array[VNode](h("!", "Text2"), h("span", "Span")))
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(elm1.childNodes(0).textContent, "Text")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.childNodes(0).textContent, "Text2")
  }

  vnode0.test(
    "updated children without keys - handles changing empty comment"
  ) { vnode0 =>
    val vnode1 = h("div", Array[VNode](h("!"), h("span", "Span")))
    val vnode2 = h("div", Array[VNode](h("!", "Test"), h("span", "Span")))
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(elm1.childNodes(0).textContent, "")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.childNodes(0).textContent, "Test")
  }

  vnode0.test("updated children without keys - prepends element") { vnode0 =>
    val vnode1 = h("div", Array(h("span", "World")))
    val vnode2 = h("div", Array(h("span", "Hello"), h("span", "World")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm1.children.toSeq.map(_.innerHTML), List("World"))
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
  }

  vnode0.test(
    "updated children without keys - prepends element of different tag type"
  ) { vnode0 =>
    val vnode1 = h("div", Array(h("span", "World")))
    val vnode2 = h("div", Array(h("div", "Hello"), h("span", "World")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm1.children.toSeq.map(_.innerHTML), List("World"))
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
    assertEquals(elm2.children.toSeq.map(_.tagName), List("DIV", "SPAN"))
  }

  vnode0.test(
    "updated children without keys - removes elements"
  ) { vnode0 =>
    val vnode1 =
      h("div", Array(h("span", "One"), h("span", "Two"), h("span", "Three")))
    val vnode2 = h("div", Array(h("span", "One"), h("span", "Three")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(
      elm1.children.toSeq.map(_.innerHTML),
      List("One", "Two", "Three")
    )
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.children.toSeq.map(_.innerHTML), List("One", "Three"))
  }

  vnode0.test(
    "updated children without keys - removes a single text node"
  ) { vnode0 =>
    val vnode1 = h("div", "One")
    val vnode2 = h("div")
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(elm1.textContent, "One")
    val elm2 = patch(vnode1, vnode2).elm.get
    assertEquals(elm2.textContent, "")
  }

  vnode0.test(
    "updated children without keys - removes a single text node when children are updated"
  ) { vnode0 =>
    val vnode1 = h("div", "One")
    val vnode2 = h("div", Array(h("div", "Two"), h("span", "Three")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm1.textContent, "One")
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.childNodes.toSeq.map(_.textContent), List("Two", "Three"))
  }

  vnode0.test(
    "updated children without keys - removes a text node among other elements"
  ) { vnode0 =>
    val vnode1 = h("div", Array[VNode]("One", h("span", "Two")))
    val vnode2 = h("div", Array(h("div", "Three")))
    val elm1 = patch(vnode0, vnode1).elm.get
    assertEquals(
      elm1.childNodes.toSeq.map(_.textContent),
      List("One", "Two")
    )
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.childNodes.length, 1)
    assertEquals(elm2.childNodes(0).asInstanceOf[dom.Element].tagName, "DIV")
    assertEquals(elm2.childNodes(0).textContent, "Three")
  }

  vnode0.test(
    "updated children without keys - reorders elements"
  ) { vnode0 =>
    val vnode1 =
      h("div", Array(h("span", "One"), h("div", "Two"), h("b", "Three")))
    val vnode2 =
      h("div", Array(h("b", "Three"), h("span", "One"), h("div", "Two")))
    val elm1 = patch(vnode0, vnode1).elm.get.asInstanceOf[dom.Element]
    assertEquals(
      elm1.children.toSeq.map(_.innerHTML),
      List("One", "Two", "Three")
    )
    val elm2 = patch(vnode1, vnode2).elm.get.asInstanceOf[dom.Element]
    assertEquals(elm2.children.toSeq.map(_.tagName), List("B", "SPAN", "DIV"))
    assertEquals(
      elm2.children.toSeq.map(_.innerHTML),
      List("Three", "One", "Two")
    )
  }

}

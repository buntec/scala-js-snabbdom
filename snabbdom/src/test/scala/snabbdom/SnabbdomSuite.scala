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

import snabbdom.modules._

import org.scalajs.dom
import scalajs.js
import scala.collection.mutable.ListBuffer

import org.scalacheck.Gen
import org.scalacheck.Gen.lzy
import org.scalacheck.rng.Seed

class SnabbdomSuite extends BaseSuite {

  // used only for key generation to ensure that the same
  // sequence of keys is used for different vnodes
  val keyRng = new scala.util.Random(0)

  val genClasses: Gen[Map[String, Boolean]] = Gen.mapOfN(
    3,
    for {
      n <- Gen.choose(5, 10)
      name <- Gen.stringOfN(n, Gen.alphaChar)
      value <- Gen.oneOf(true, false)
    } yield (name, value)
  )

  val genProps: Gen[Map[String, String]] = Gen.mapOfN(
    3,
    for {
      name <- Gen.stringOfN(10, Gen.alphaChar)
      value <- Gen.stringOfN(10, Gen.alphaChar)
    } yield (name, value)
  )

  val genVNodeData: Gen[VNodeData] = {
    // dirty hack to allow temporary switching of rngs
    var globalSeed: Seed = null
    for {
      key <- Gen
        .frequency(
          (1 -> Gen.const(None)),
          (1 -> Gen.some(Gen.uuid.map(_.toString.take(8))))
        )
        .withPerturb { seed =>
          globalSeed = seed
          Seed(keyRng.nextLong()) // use seed driven by key rng
        }
      _ <- Gen.const(()).withPerturb(_ => globalSeed) // back to global seed
    } yield {
      VNodeData(key = key)
    }
  }

  val genLeaf: Gen[VNode] = for {
    n <- Gen.choose(3, 20)
    text <- Gen.stringOfN(n, Gen.alphaChar)
    data <- genVNodeData
  } yield h("span", data, text)

  val genTree: Gen[VNode] = for {
    children <- Gen.listOfN(8, genVNodePre)
    data <- genVNodeData
  } yield h("div", data, children)

  // Dies out fast enough not to explode, but has 10-1 odds of being just a leaf.
  // We therefore define `genVNode` by conditioning on `size(vnode) > 1`.
  def genVNodePre: Gen[VNode] =
    Gen.frequency((10, genLeaf), (1, lzy(genTree)))

  // We reset the key rng after every evaluation so that
  // successive vnodes will use the same sequence of keys
  val genVNode: Gen[VNode] =
    genVNodePre
      .flatMap { vnode =>
        Gen.delay { keyRng.setSeed(0); Gen.const(vnode) }
      }
      .retryUntil(vnode => size(vnode) > 1)

  def size(vnode: VNode): Long = 1 + vnode.children.map(size).sum

  def keys(vnode: VNode): Set[String] =
    vnode.key.toSet.union(vnode.children.map(keys).toList.flatten.toSet)

  def depth(vnode: VNode): Long =
    1 + vnode.children.map(depth).maxOption.getOrElse(0L)

  def spanNum(s: String) = h("span", VNodeData(), s)
  def spanNum(i: Int) =
    h("span", VNodeData(key = Some(i.toString)), i.toString)

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

  group("hyperscript") {
    test("can create vnode with proper tag") {
      assertEquals(h("div").sel, Some("div"))
      assertEquals(h("a").sel, Some("a"))
    }

    test("can create vnode with children") {
      val vnode = h("div", List(h("span#hello"), h("b.world")))
      assertEquals(vnode.sel, Some("div"))
      val children = vnode.children
      assertEquals(children.head.sel, Some("span#hello"))
      assertEquals(children.tail.head.sel, Some("b.world"))
    }

    test("can create vnode with one child vnode") {
      val vnode = h("div", List(h("span#hello")))
      assertEquals(vnode.sel, Some("div"))
      val children = vnode.children
      assertEquals(children.head.sel, Some("span#hello"))
    }

    test("can create vnode with props and one child vnode") {
      val vnode = h("div", VNodeData(), h("span#hello"))
      assertEquals(vnode.sel, Some("div"))
      val children = vnode.children
      assertEquals(children.head.sel, Some("span#hello"))
    }

    test("can create vnode with text content") {
      val vnode = h("a", List(VNode.text("I am a string")))
      val children = vnode.children
      assertEquals(children.head.text, Some("I am a string"))
    }

    test("can create vnode with text content in string") {
      val vnode = h("a", "I am a string")
      assertEquals(vnode.text, Some("I am a string"))
    }

    test("can create vnode with props and text content in string") {
      val vnode = h("a", VNodeData(), "I am a string")
      assertEquals(vnode.text, Some("I am a string"))
    }

    test("can create vnode with String obj content") {
      val vnode = h("a", new String("b"))
      assertEquals(vnode.text, Some("b"))
    }

    test("can create vnode with props and String obj content") {
      val vnode = h("a", VNodeData(), new String("b"))
      assertEquals(vnode.text, Some("b"))
    }

    test("can create vnode for comment") {
      val vnode = h("!", "test")
      assertEquals(vnode.sel, Some("!"))
      assertEquals(vnode.text, Some("test"))
    }
  }

  group("created element") {
    vnode0.test("has tag") { vnode0 =>
      val elm = patch(vnode0, h("div")).elm.asInstanceOf[dom.Element]
      assertEquals(elm.tagName, "DIV")
    }

    vnode0.test("has different tag and id") { vnode0 =>
      val elm = dom.document.createElement("div")
      vnode0.appendChild(elm)
      val vnode1 = h("span#id")
      val patched = patch(elm, vnode1).elm.asInstanceOf[dom.HTMLSpanElement]
      assertEquals(patched.tagName, "SPAN")
      assertEquals(patched.id, "id")
    }

    vnode0.test("has id") { vnode0 =>
      val elm = patch(vnode0, h("div", List(h("div#unique")))).elm
      assertEquals(elm.firstChild.asInstanceOf[dom.Element].id, "unique")
    }

    vnode0.test("has correct namespace") { vnode0 =>
      val SVGNamespace = "http://www.w3.org/2000/svg";
      val XHTMLNamespace = "http://www.w3.org/1999/xhtml";

      val data = VNodeData(ns = Some(SVGNamespace))

      val elm1 = patch(vnode0, h("div", List(h("div", data)))).elm
      assertEquals(elm1.firstChild.namespaceURI, SVGNamespace)

      // verify that svg tag automatically gets svg namespace
      val elm2 = patch(
        vnode0,
        h(
          "svg",
          List(
            h(
              "foreignObject",
              List(h("div", List[VNode]("I am HTML embedded in SVG")))
            )
          )
        )
      ).elm

      assertEquals(elm2.namespaceURI, SVGNamespace)
      assertEquals(elm2.firstChild.namespaceURI, SVGNamespace)
      assertEquals(elm2.firstChild.firstChild.namespaceURI, XHTMLNamespace)

      // verify that svg tag with extra selectors gets svg namespace
      val elm3 = patch(vnode0, h("svg#some-id")).elm
      assertEquals(elm3.namespaceURI, SVGNamespace);

      // verify that non-svg tag beginning with 'svg' does NOT get namespace
      val elm4 = patch(vnode0, h("svg-custom-el")).elm
      assertNotEquals(elm4.namespaceURI, SVGNamespace)

    }

    vnode0.test("receives classes in selector") { vnode0 =>
      val elm = patch(vnode0, h("div", List(h("i.am.a.class")))).elm
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("am"))
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
      assert(
        elm.firstChild.asInstanceOf[dom.Element].classList.contains("class")
      )
    }

    vnode0.test("receives classes in class property") { vnode0 =>
      val data = VNodeData(classes =
        Map(
          "am" -> true,
          "a" -> true,
          "class" -> true,
          "not" -> false
        )
      )
      val elm = patch(vnode0, h("i", data)).elm
      assert(elm.asInstanceOf[dom.Element].classList.contains("am"))
      assert(elm.asInstanceOf[dom.Element].classList.contains("a"))
      assert(elm.asInstanceOf[dom.Element].classList.contains("class"))
      assert(!elm.asInstanceOf[dom.Element].classList.contains("not"))
    }

    vnode0.test("receives classes in selector when namespaced") { vnode0 =>
      val elm = patch(vnode0, h("svg", List(h("g.am.a.class.too")))).elm
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("am"))
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
      assert(
        elm.firstChild.asInstanceOf[dom.Element].classList.contains("class")
      )
    }

    vnode0.test("receives classes in class property when namespaced") {
      vnode0 =>
        val data = VNodeData(classes =
          Map(
            "am" -> true,
            "a" -> true,
            "class" -> true,
            "not" -> false
          )
        )
        val elm = patch(vnode0, h("svg", List(h("g", data)))).elm
        assert(
          elm.firstChild.asInstanceOf[dom.Element].classList.contains("am")
        )
        assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("a"))
        assert(
          elm.firstChild.asInstanceOf[dom.Element].classList.contains("class")
        )
        assert(
          !elm.firstChild.asInstanceOf[dom.Element].classList.contains("not")
        )
    }

    vnode0.test("handles classes from both selector and property") { vnode0 =>
      val data = VNodeData(classes = Map("classes" -> true))
      val elm = patch(vnode0, h("div", List(h("i.has", data)))).elm
      assert(elm.firstChild.asInstanceOf[dom.Element].classList.contains("has"))
      assert(
        elm.firstChild.asInstanceOf[dom.Element].classList.contains("classes")
      )
    }

    vnode0.test("can create elements with text content") { vnode0 =>
      val elm = patch(vnode0, h("div", List[VNode]("I am a string"))).elm
      assertEquals(elm.asInstanceOf[dom.Element].innerHTML, "I am a string")
    }

    vnode0.test("can create elements with span and text content") { vnode0 =>
      val elm =
        patch(vnode0, h("a", List[VNode](h("span"), "I am a string"))).elm
      assertEquals(elm.childNodes(0).asInstanceOf[dom.Element].tagName, "SPAN")
      assertEquals(
        elm.childNodes(1).asInstanceOf[dom.Element].textContent,
        "I am a string"
      )
    }

    vnode0.test("can create vnode with array String obj content") { vnode0 =>
      val elm = patch(vnode0, h("a", List[VNode]("b", "c"))).elm
      assertEquals(elm.asInstanceOf[dom.Element].innerHTML, "bc")
    }

    vnode0.test("can create elements with props") { vnode0 =>
      val data = VNodeData(props = Map("src" -> "http://localhost/"))
      val elm = patch(vnode0, h("a", data)).elm
      assertEquals(
        elm.asInstanceOf[js.Dictionary[String]]("src"),
        "http://localhost/"
      )
    }

    vnode0.test("can create and element created inside an iframe") { _ =>
      // TODO
    }

    test("is a patch of the root element") {
      val elmWithIdAndClass = dom.document.createElement("div")
      elmWithIdAndClass.id = "id"
      elmWithIdAndClass.asInstanceOf[dom.HTMLElement].className = "class"
      val vnode1 = h("div#id.class", List(h("span", "Hi")));
      val elm =
        patch(elmWithIdAndClass, vnode1).elm.asInstanceOf[dom.Element]
      assertEquals(elm, elmWithIdAndClass)
      assertEquals(elm.tagName, "DIV")
      assertEquals(elm.id, "id")
      assertEquals(elm.asInstanceOf[dom.HTMLElement].className, "class")
    }

    vnode0.test("can create comments") { vnode0 =>
      val elm = patch(vnode0, h("!", "test")).elm
      assertEquals(elm.nodeType, dom.Node.COMMENT_NODE)
      assertEquals(elm.textContent, "test")
    }

  }

  group("created document fragment") {

    vnode0.test("is an instance of DocumentFragment") { vnode0 =>
      val vnode1 =
        fragment(
          List[VNode]("I am", h("span", List[VNode](" a", " fragment")))
        )
      val elm = patch(vnode0, vnode1).elm
      assertEquals(elm.nodeType, dom.Node.DOCUMENT_FRAGMENT_NODE)
      assertEquals(elm.textContent, "I am a fragment")
    }

  }

  group("patching an element") {
    vnode0.test("changes the elements classes") { vnode0 =>
      val vnode1 = h(
        "i",
        VNodeData(classes = Map("i" -> true, "am" -> true, "horse" -> true))
      )
      val vnode2 = h(
        "i",
        VNodeData(classes = Map("i" -> true, "am" -> true, "horse" -> false))
      )
      val vnode1p = patch(vnode0, vnode1)
      val elm = patch(vnode1p, vnode2).elm
      assert(elm.asInstanceOf[dom.Element].classList.contains("i"))
      assert(elm.asInstanceOf[dom.Element].classList.contains("am"))
      assert(!elm.asInstanceOf[dom.Element].classList.contains("horse"))
    }

    vnode0.test("changes classes in selector") { _ =>
      // TODO: looks exactly like the the one above in the original???
    }

    vnode0.test("preserves memoized classes") { vnode0 =>
      val cachedClasses =
        VNodeData(classes = Map("i" -> true, "am" -> true, "horse" -> false))
      val vnode1 = h("i", cachedClasses)
      val vnode2 = h("i", cachedClasses)
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm.asInstanceOf[dom.Element]
      assert(elm.classList.contains("i"))
      assert(elm.classList.contains("am"))
      assert(!elm.classList.contains("horse"))
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assert(elm2.classList.contains("i"))
      assert(elm2.classList.contains("am"))
      assert(!elm2.classList.contains("horse"))
    }

    vnode0.test("removes missing classes") { vnode0 =>
      val vnode1 = h(
        "i",
        VNodeData(classes = Map("i" -> true, "am" -> true, "horse" -> true))
      )
      val vnode2 = h(
        "i",
        VNodeData(classes = Map("i" -> true, "am" -> true))
      )
      val vnode1p = patch(vnode0, vnode1)
      val elm = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assert(elm.classList.contains("i"))
      assert(elm.classList.contains("am"))
      assert(!elm.classList.contains("horse"))
    }

    vnode0.test("changes an elements props") { vnode0 =>
      val vnode1 =
        h("a", VNodeData(props = Map("src" -> "http://other/")))
      val vnode2 =
        h("a", VNodeData(props = Map("src" -> "http://localhost/")))
      val vnode1p = patch(vnode0, vnode1)
      val elm =
        patch(vnode1p, vnode2).elm.asInstanceOf[js.Dictionary[String]]
      assertEquals(elm("src"), "http://localhost/")
    }

    vnode0.test("preserves memoized props") { vnode0 =>
      val cachedProps = VNodeData(props = Map("src" -> "http://other/"))
      val vnode1 = h("a", cachedProps)
      val vnode2 = h("a", cachedProps)
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(
        elm.asInstanceOf[js.Dictionary[String]]("src"),
        "http://other/"
      )
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(
        elm2.asInstanceOf[js.Dictionary[String]]("src"),
        "http://other/"
      )
    }

    vnode0.test("can set prop value to empty string") { vnode0 =>
      val vnode1 =
        h("p", VNodeData(props = Map("textContent" -> "foo")))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(
        elm.asInstanceOf[dom.HTMLParagraphElement].textContent,
        "foo"
      )
      val vnode2 =
        h("p", VNodeData(props = Map("textContent" -> "")))
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.HTMLParagraphElement].textContent, "")

    }

    vnode0.test("removes custom props") { vnode0 =>
      val vnode1 =
        h("a", VNodeData(props = Map("src" -> "http://other/")))
      val vnode2 = h("a")
      val vnode1p = patch(vnode0, vnode1)
      val elm = patch(vnode1p, vnode2).elm
      assert(!elm.asInstanceOf[js.Dictionary[String]].contains("src"))
    }

    vnode0.test("cannot remove native props") { vnode0 =>
      val vnode1 =
        h("a", VNodeData(props = Map("href" -> "http://example.com/")))
      val vnode2 = h("a")
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assert(elm1.isInstanceOf[dom.HTMLAnchorElement])
      assertEquals(
        elm1.asInstanceOf[dom.HTMLAnchorElement].href,
        "http://example.com/"
      )
      val elm2 = patch(vnode1p, vnode2).elm
      assert(elm2.isInstanceOf[dom.HTMLAnchorElement])
      assertEquals(
        elm2.asInstanceOf[dom.HTMLAnchorElement].href,
        "http://example.com/"
      )

    }

    // this test seems to assert the opposite of "removes custom props" ???
    vnode0.test("does not delete custom props".ignore) { vnode0 =>
      val vnode1 = h("p", VNodeData(props = Map("a" -> "foo")))
      val vnode2 = h("p")
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assert(elm.isInstanceOf[dom.HTMLParagraphElement])
      assertEquals(elm.asInstanceOf[js.Dictionary[String]]("a"), "foo")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[js.Dictionary[String]]("a"), "foo")
    }
  }

  group("using `toVNode`") {

    test("can remove previous children of the root element") {
      val h2 = dom.document.createElement("h2")
      h2.textContent = "Hello"
      val prevElm =
        dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      prevElm.id = "id"
      prevElm.className = "class"
      prevElm.appendChild(h2)
      val nextVNode = h("div#id.class", List(h("span", "Hi")))
      val elm =
        patch(toVNode(prevElm), nextVNode).elm
          .asInstanceOf[dom.HTMLElement]
      assertEquals(elm, prevElm)
      assertEquals(elm.tagName, "DIV")
      assertEquals(elm.id, "id")
      assertEquals(elm.className, "class")
      assertEquals(elm.childNodes.length, 1)
      assertEquals(
        elm.childNodes(0).asInstanceOf[dom.HTMLElement].tagName,
        "SPAN"
      )
      assertEquals(
        elm.childNodes(0).textContent,
        "Hi"
      )
    }

    test("can support patching in a DocumentFragment") {
      val prevElm = dom.document.createDocumentFragment()
      val nextVNode = VNode.create(
        Some(""),
        VNodeData.empty,
        List(h("div#id.class", List(h("span", "Hi")))),
        None
        // Some(prevElm)
      )
      val elm =
        patch(toVNode(prevElm), nextVNode).elm
          .asInstanceOf[dom.DocumentFragment]
      assertEquals(elm, prevElm)
      assertEquals(elm.nodeType, 11)
      assertEquals(elm.childNodes.length, 1)
      assertEquals(
        elm.childNodes(0).asInstanceOf[dom.HTMLElement].tagName,
        "DIV"
      )
      assertEquals(elm.childNodes(0).asInstanceOf[dom.HTMLElement].id, "id")
      assertEquals(
        elm.childNodes(0).asInstanceOf[dom.HTMLElement].className,
        "class"
      )
      assertEquals(
        elm.childNodes(0).asInstanceOf[dom.HTMLElement].childNodes.length,
        1
      )
      assertEquals(
        elm
          .childNodes(0)
          .asInstanceOf[dom.HTMLElement]
          .childNodes(0)
          .asInstanceOf[dom.HTMLElement]
          .tagName,
        "SPAN"
      )
      assertEquals(
        elm
          .childNodes(0)
          .asInstanceOf[dom.HTMLElement]
          .childNodes(0)
          .asInstanceOf[dom.HTMLElement]
          .textContent,
        "Hi"
      )
    }

    test("can remove some children of the root element") {
      val h2 = dom.document.createElement("h2")
      h2.textContent = "Hello"
      val prevElm =
        dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      prevElm.id = "id"
      prevElm.className = "class"
      val text = dom.document.createTextNode("Foobar")
      val reference = js.Object()
      // ensure that we don't recreate the Text node
      text.asInstanceOf[js.Dictionary[Any]]("testProperty") = reference
      prevElm.appendChild(text)
      prevElm.appendChild(h2)
      val nextVNode = h("div#id.class", List[VNode]("Foobar"))
      val elm =
        patch(toVNode(prevElm), nextVNode).elm.asInstanceOf[dom.HTMLElement]
      assertEquals(elm, prevElm)
      assertEquals(elm.tagName, "DIV")
      assertEquals(elm.id, "id")
      assertEquals(elm.className, "class")
      assertEquals(elm.childNodes.length, 1)
      assertEquals(elm.childNodes(0).nodeType, 3)
      assertEquals(elm.childNodes(0).asInstanceOf[dom.Text].wholeText, "Foobar")
      assertEquals(
        elm.childNodes(0).asInstanceOf[js.Dictionary[Any]]("testProperty"),
        reference
      )
    }

    test("can remove text elements") {

      val h2 = dom.document.createElement("h2")
      h2.textContent = "Hello"
      val prevElm =
        dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      prevElm.id = "id"
      prevElm.className = "class"
      val text = dom.document.createTextNode("Foobar")
      prevElm.appendChild(text)
      prevElm.appendChild(h2)
      val nextVNode = h("div#id.class", List(h("h2", "Hello")))
      val elm =
        patch(toVNode(prevElm), nextVNode).elm.asInstanceOf[dom.HTMLElement]

      assertEquals(elm, prevElm)
      assertEquals(elm.tagName, "DIV")
      assertEquals(elm.id, "id")
      assertEquals(elm.className, "class")
      assertEquals(elm.childNodes.length, 1)
      assertEquals(elm.childNodes(0).nodeType, 1)
      assertEquals(elm.childNodes(0).textContent, "Hello")

    }

    test("can work with DomApi") {
      // TODO
    }

    test("can parse datasets and attrs") {

      val onlyAttrs = dom.document.createElement("div")
      onlyAttrs.setAttribute("foo", "bar")
      assertEquals(
        toVNode(onlyAttrs).data.attrs.get("foo"),
        Some("bar")
      )

      val onlyDatasets = dom.document.createElement("div")
      onlyDatasets.setAttribute("data-foo", "bar")
      assertEquals(
        toVNode(onlyDatasets).data.dataset.get("foo"),
        Some("bar")
      )

      val onlyDatasets2 =
        dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      onlyDatasets2.dataset("foo") = "bar"
      assertEquals(
        toVNode(onlyDatasets).data.dataset.get("foo"),
        Some("bar")
      )

      val bothAttrsAndDatasets =
        dom.document.createElement("div").asInstanceOf[dom.HTMLElement]
      bothAttrsAndDatasets.setAttribute("foo", "bar")
      bothAttrsAndDatasets.setAttribute("data-foo", "bar")
      bothAttrsAndDatasets.dataset("again") = "again"
      val data = toVNode(bothAttrsAndDatasets).data
      assertEquals(data.attrs.get("foo"), Some("bar"))
      assertEquals(data.dataset.get("foo"), Some("bar"))
      assertEquals(data.dataset.get("again"), Some("again"))

    }

  }

  group("addition of elements") {

    vnode0.test("appends elements") { vnode0 =>
      val vnode1 = h("span", List("1").map(spanNum))
      val vnode2 = h("span", List("1", "2", "3").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 1)
      val vnode12 = patch(vnode1p, vnode2)
      val elm2 = vnode12.elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
      assertEquals(elm2.asInstanceOf[dom.Element].children(1).innerHTML, "2")
      assertEquals(elm2.asInstanceOf[dom.Element].children(2).innerHTML, "3")
    }

    vnode0.test("prepends elements") { vnode0 =>
      val vnode1 = h("span", List("4", "5").map(spanNum))
      val vnode2 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 2)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3", "4", "5")
      )
    }

    vnode0.test("add elements in the middle") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "4", "5").map(spanNum))
      val vnode2 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3", "4", "5")
      )
    }

    vnode0.test("add elements at begin and end") { vnode0 =>
      val vnode1 = h("span", List("2", "3", "4").map(spanNum))
      val vnode2 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3", "4", "5")
      )
    }

    vnode0.test("adds children to parent with no children") { vnode0 =>
      val vnode1 = h("span", VNodeData(key = Some("span")))
      val vnode2 = h(
        "span",
        VNodeData(key = Some("span")),
        List("1", "2", "3").map(spanNum)
      )
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 0)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3")
      )
    }

    vnode0.test("removes all children to parent") { vnode0 =>
      val vnode1 = h(
        "span",
        VNodeData(key = Some("span")),
        List("1", "2", "3").map(spanNum)
      )
      val vnode2 = h("span", VNodeData(key = Some("span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(
        elm.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3")
      )
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 0)
    }

    vnode0.test("update one child with same key but different sel") { vnode0 =>
      val data = VNodeData(key = Some("span"))
      val data2 = VNodeData(key = Some("2"))
      val vnode1 = h("span", data, List("1", "2", "3").map(spanNum))
      val vnode2 =
        h("span", data, List(spanNum("1"), h("i", data2, "2"), spanNum("3")))

      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(
        elm.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3")
      )

      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3")
      )

      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
      assertEquals(elm2.asInstanceOf[dom.Element].children(1).tagName, "I")

    }
  }

  group("removal of elements") {
    vnode0.test("removes elements from the beginning") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", List("3", "4", "5").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("3", "4", "5")
      )
    }

    vnode0.test("removes elements from the end") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", List("1", "2", "3").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "3")
      )
    }

    vnode0.test("removes elements from the middle") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", List("1", "2", "4", "5").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "2", "4", "5")
      )
    }
  }

  group("element reordering") {
    vnode0.test("moves element forward") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4").map(spanNum))
      val vnode2 = h("span", List("2", "3", "1", "4").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("2", "3", "1", "4")
      )
    }

    vnode0.test("moves element to end") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3").map(spanNum))
      val vnode2 = h("span", List("2", "3", "1").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("2", "3", "1")
      )
    }

    vnode0.test("moves element backwards") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4").map(spanNum))
      val vnode2 = h("span", List("1", "4", "2", "3").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("1", "4", "2", "3")
      )
    }

    vnode0.test("swaps first and last") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4").map(spanNum))
      val vnode2 = h("span", List("4", "2", "3", "1").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("4", "2", "3", "1")
      )
    }

    vnode0.test("reverses order of children twice") { vnode0 =>
      val vnode1 = h("div", List(h("span", "1"), h("span", "2")))
      val vnode2 = h("div", List(h("span", "2"), h("span", "1")))
      val vnode3 = h("div", List(h("span", "1"), h("span", "2")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.asInstanceOf[dom.Element].children(0).innerHTML, "1")
      assertEquals(elm1.asInstanceOf[dom.Element].children(1).innerHTML, "2")
      val vnode2p = patch(vnode1p, vnode2)
      val elm2 = vnode2p.elm
      assertEquals(elm2.asInstanceOf[dom.Element].children(0).innerHTML, "2")
      assertEquals(elm2.asInstanceOf[dom.Element].children(1).innerHTML, "1")
      val vnode3p = patch(vnode2p, vnode3)
      val elm3 = vnode3p.elm
      assertEquals(elm3.asInstanceOf[dom.Element].children(0).innerHTML, "1")
      assertEquals(elm3.asInstanceOf[dom.Element].children(1).innerHTML, "2")
    }

  }

  group("combinations of additions, removals and reorderings") {
    vnode0.test("move to left and replace") { vnode0 =>
      val vnode1 = h("span", List("1", "2", "3", "4", "5").map(spanNum))
      val vnode2 = h("span", List("4", "1", "2", "3", "6").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 5)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 5)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("4", "1", "2", "3", "6")
      )
    }

    vnode0.test("moves to left and leaves hole") { vnode0 =>
      val vnode1 = h("span", List("1", "4", "5").map(spanNum))
      val vnode2 = h("span", List("4", "6").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 2)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("4", "6")
      )
    }

    vnode0.test(
      "handles moved and set to undefined element ending at the end"
    ) { vnode0 =>
      val vnode1 = h("span", List("2", "4", "5").map(spanNum))
      val vnode2 = h("span", List("4", "5", "3").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 3)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 3)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("4", "5", "3")
      )
    }

    vnode0.test("moves a key in non-keyed nodes with a size up") { vnode0 =>
      val vnode1 =
        h("span", List(spanNum(1)) ++ List("a", "b", "c").map(spanNum))
      val vnode2 = h(
        "span",
        List("d", "a", "b", "c").map(spanNum) ++ List(spanNum(1)) ++ List(
          spanNum("e")
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 4)
      assertEquals(elm.textContent, "1abc")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 6)
      assertEquals(elm2.textContent, "dabc1e")
    }

    // accepts symbol as key - doesn't apply to Scala.js

    vnode0.test("reverses elements") { vnode0 =>
      val vnode1 =
        h("span", List("1", "2", "3", "4", "5", "6", "7", "8").map(spanNum))
      val vnode2 =
        h("span", List("8", "7", "6", "5", "4", "3", "2", "1").map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 8)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 8)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("8", "7", "6", "5", "4", "3", "2", "1")
      )
    }

    vnode0.test("something") { vnode0 =>
      val vnode1 =
        h("span", List(0, 1, 2, 3, 4, 5).map(spanNum))
      val vnode2 =
        h("span", List(4, 3, 2, 1, 5, 0).map(spanNum))
      val vnode1p = patch(vnode0, vnode1)
      val elm = vnode1p.elm
      assertEquals(elm.asInstanceOf[dom.Element].children.length, 6)
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.asInstanceOf[dom.Element].children.length, 6)
      assertEquals(
        elm2.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
        List("4", "3", "2", "1", "5", "0")
      )
    }

    def spanNumWithOpacity(n: Int, o: String) = {
      h(
        "span",
        VNodeData(key = Some(n.toString), style = Map("opacity" -> o)),
        n.toString
      )
    }

    // it is odd that the orginal passes without using the styles module...
    test("handles random shuffles") {
      val elms = 14
      val samples = 50
      val arr = List.tabulate(elms)(i => i)
      val rng = new scala.util.Random(0)
      (0 until samples).foreach { _ =>
        val vnode1 = h("span", arr.map(spanNumWithOpacity(_, "1")))
        val shufArr = rng.shuffle(arr)
        val elm = dom.document.createElement("div")
        val vnode1p = patch(elm, vnode1)
        val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLSpanElement]
        assertEquals(
          elm1.asInstanceOf[dom.Element].children.toList.map(_.innerHTML),
          arr.map(_.toString).toList
        )
        val opacities = List.tabulate(elms)(_ => f"${rng.nextDouble()}%.5f")
        val vnode2 =
          h("span", arr.map(n => spanNumWithOpacity(shufArr(n), opacities(n))))
        val elm2 =
          patch(vnode1p, vnode2).elm.asInstanceOf[dom.HTMLSpanElement]
        (0 until elms).foreach { i =>
          assertEquals(elm2.children(i).innerHTML, shufArr(i).toString)
          val opacity =
            elm2.children(i).asInstanceOf[dom.HTMLSpanElement].style.opacity
          assertEquals(opacity.toDouble, opacities(i).toDouble)
        }
      }
    }

    test("random shuffle of keyed and non-keyed nodes") {

      val rng = new scala.util.Random(0)

      def span(text: String) = h("span", text)

      def kSpan(text: String, key: Int, update: UpdateHook) =
        h(
          "span",
          VNodeData(
            key = Some(key.toString),
            hook = Some(Hooks(update = Some(update)))
          ),
          text
        )

      for {
        _ <- 0 until 1000
      } yield {

        val n1 = rng.nextInt(10)
        val n2 = rng.nextInt(10)

        var counter = 0

        val cb: UpdateHook = (_, vnode) => {
          counter += 1
          vnode
        }

        val spans = List.tabulate(n1)(i => span(s"a-$i"))
        val kspans = List.tabulate(n2)(i => kSpan(s"k-$i", i, cb))

        val spansAll = spans ::: kspans

        val vnode1 = h("div", rng.shuffle(spansAll))
        val vnode2 = h(
          "div",
          rng
            .shuffle(spansAll)
            .map(vnode => vnode.copy(text = vnode.text.map(_ + "X")))
        )
        val vnode3 = h("div", rng.shuffle(spansAll))

        val elm = dom.document.createElement("div")
        val vnode1p = patch(elm, vnode1)
        assertEquals(vnode1p.toVNode, vnode1)
        val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLElement]
        val innerHTML1 = vnode1.children.map { vnode =>
          s"<span>${vnode.text.get}</span>"
        }.mkString
        assertEquals(elm1.innerHTML, innerHTML1)

        val vnode2p = patch(vnode1p, vnode2)
        assertEquals(vnode2p.toVNode, vnode2)
        val elm2 = vnode2p.elm.asInstanceOf[dom.HTMLElement]
        val innerHTML2 = vnode2.children.map { vnode =>
          s"<span>${vnode.text.get}</span>"
        }.mkString
        assertEquals(elm2.innerHTML, innerHTML2)

        val vnode3p = patch(vnode2p, vnode3)
        assertEquals(vnode3p.toVNode, vnode3)
        val elm3 = vnode3p.elm.asInstanceOf[dom.HTMLElement]
        val innerHTML3 = vnode3.children.map { vnode =>
          s"<span>${vnode.text.get}</span>"
        }.mkString
        assertEquals(elm3.innerHTML, innerHTML3)

        // update hook should trigger twice, in patch from 1 -> 2 and from 2 -> 3
        assertEquals(counter, 2 * n2)

      }

    }

    vnode0.test("keyed nodes are patched") { vnode0 =>
      val result1b = List.newBuilder[VNode]
      val result2b = List.newBuilder[VNode]
      val cb: UpdateHook = (pvnode, newVNode) => {
        result1b += pvnode.toVNode
        result2b += newVNode
        newVNode
      }

      def keyedSpan(text: String, key: Int) =
        h(
          "span",
          VNodeData(
            key = Some(key.toString),
            hook = Some(Hooks(update = Some(cb)))
          ),
          text
        )

      val vnode1 =
        h(
          "div",
          List(
            h("span", "a"),
            keyedSpan("a", 1),
            keyedSpan("b", 2),
            keyedSpan("c", 3),
            h("span", "d")
          )
        )

      val vnode2 = h(
        "div",
        List(
          keyedSpan("aa", 1),
          keyedSpan("bb", 2),
          keyedSpan("cc", 3),
          h("span", "d")
        )
      )

      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)

      val result1 = result1b.result()
      val result2 = result2b.result()
      assertEquals(
        result1.toSet,
        Set(
          keyedSpan("a", 1),
          keyedSpan("b", 2),
          keyedSpan("c", 3)
        )
      )
      assertEquals(
        result2.toSet,
        Set(
          keyedSpan("aa", 1),
          keyedSpan("bb", 2),
          keyedSpan("cc", 3)
        )
      )

    }

  }

  group("updated children without keys") {
    vnode0.test("appends elements") { vnode0 =>
      val vnode1 = h("div", List(h("span", "Hello")))
      val vnode2 = h("div", List(h("span", "Hello"), h("span", "World")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
      assertEquals(elm1.children.toSeq.map(_.innerHTML), List("Hello"))
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
    }

    vnode0.test("handles unmoved text nodes") { vnode0 =>
      val vnode1 = h("div", List[VNode]("Text", h("span", "Span")))
      val vnode2 = h("div", List[VNode]("Text", h("span", "Span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.childNodes(0).textContent, "Text")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.childNodes(0).textContent, "Text")
    }

    vnode0.test("handles changing text children") { vnode0 =>
      val vnode1 = h("div", List[VNode]("Text", h("span", "Span")))
      val vnode2 = h("div", List[VNode]("Text2", h("span", "Span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.childNodes(0).textContent, "Text")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.childNodes(0).textContent, "Text2")
    }

    vnode0.test("handles unmoved comment nodes") { vnode0 =>
      val vnode1 = h("div", List[VNode](h("!", "Text"), h("span", "Span")))
      val vnode2 = h("div", List[VNode](h("!", "Text"), h("span", "Span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.childNodes(0).textContent, "Text")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.childNodes(0).textContent, "Text")
    }

    vnode0.test("handles changing comment text") { vnode0 =>
      val vnode1 = h("div", List[VNode](h("!", "Text"), h("span", "Span")))
      val vnode2 = h("div", List[VNode](h("!", "Text2"), h("span", "Span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.childNodes(0).textContent, "Text")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.childNodes(0).textContent, "Text2")
    }

    vnode0.test("handles changing empty comment") { vnode0 =>
      val vnode1 = h("div", List[VNode](h("!"), h("span", "Span")))
      val vnode2 = h("div", List[VNode](h("!", "Test"), h("span", "Span")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.childNodes(0).textContent, "")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.childNodes(0).textContent, "Test")
    }

    vnode0.test("prepends element") { vnode0 =>
      val vnode1 = h("div", List(h("span", "World")))
      val vnode2 = h("div", List(h("span", "Hello"), h("span", "World")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
      assertEquals(elm1.children.toSeq.map(_.innerHTML), List("World"))
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
    }

    vnode0.test("prepends element of different tag type") { vnode0 =>
      val vnode1 = h("div", List(h("span", "World")))
      val vnode2 = h("div", List(h("div", "Hello"), h("span", "World")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
      assertEquals(elm1.children.toSeq.map(_.innerHTML), List("World"))
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.children.toSeq.map(_.innerHTML), List("Hello", "World"))
      assertEquals(elm2.children.toSeq.map(_.tagName), List("DIV", "SPAN"))
    }

    vnode0.test("removes elements") { vnode0 =>
      val vnode1 =
        h("div", List(h("span", "One"), h("span", "Two"), h("span", "Three")))
      val vnode2 = h("div", List(h("span", "One"), h("span", "Three")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
      assertEquals(
        elm1.children.toSeq.map(_.innerHTML),
        List("One", "Two", "Three")
      )
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.children.toSeq.map(_.innerHTML), List("One", "Three"))
    }

    vnode0.test("removes a single text node") { vnode0 =>
      val vnode1 = h("div", "One")
      val vnode2 = h("div")
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(elm1.textContent, "One")
      val elm2 = patch(vnode1p, vnode2).elm
      assertEquals(elm2.textContent, "")
    }

    vnode0.test("removes a single text node when children are updated") {
      vnode0 =>
        val vnode1 = h("div", "One")
        val vnode2 = h("div", List(h("div", "Two"), h("span", "Three")))
        val vnode1p = patch(vnode0, vnode1)
        val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
        assertEquals(elm1.textContent, "One")
        val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
        assertEquals(
          elm2.childNodes.toSeq.map(_.textContent),
          List("Two", "Three")
        )
    }

    vnode0.test("removes a text node among other elements") { vnode0 =>
      val vnode1 = h("div", List[VNode]("One", h("span", "Two")))
      val vnode2 = h("div", List(h("div", "Three")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm
      assertEquals(
        elm1.childNodes.toSeq.map(_.textContent),
        List("One", "Two")
      )
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.childNodes.length, 1)
      assertEquals(elm2.childNodes(0).asInstanceOf[dom.Element].tagName, "DIV")
      assertEquals(elm2.childNodes(0).textContent, "Three")
    }

    vnode0.test("reorders elements") { vnode0 =>
      val vnode1 =
        h("div", List(h("span", "One"), h("div", "Two"), h("b", "Three")))
      val vnode2 =
        h("div", List(h("b", "Three"), h("span", "One"), h("div", "Two")))
      val vnode1p = patch(vnode0, vnode1)
      val elm1 = vnode1p.elm.asInstanceOf[dom.Element]
      assertEquals(
        elm1.children.toSeq.map(_.innerHTML),
        List("One", "Two", "Three")
      )
      val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.Element]
      assertEquals(elm2.children.toSeq.map(_.tagName), List("B", "SPAN", "DIV"))
      assertEquals(
        elm2.children.toSeq.map(_.innerHTML),
        List("Three", "One", "Two")
      )
    }
  }

  group("patching a fragment") {
    vnode0.test("can patch on document fragments") { vnode0 =>
      val vnode1 = fragment(
        List(
          "I am",
          h("span", List(VNode.text(" a"), VNode.text(" fragment")))
        )
      )
      val vnode2 = h("div", List(VNode.text("I am an element")))
      val vnode3 = fragment(List("fragment ", "again"))

      val vnode1p = patch(vnode0, vnode1)
      var elm = vnode1p.elm
      assertEquals(elm.nodeType, dom.Node.DOCUMENT_FRAGMENT_NODE)

      val vnode2p = patch(vnode1p, vnode2)
      elm = vnode2p.elm
      assertEquals(elm.asInstanceOf[dom.Element].tagName, "DIV")
      assertEquals(elm.textContent, "I am an element")

      val vnode3p = patch(vnode2p, vnode3)
      elm = vnode3p.elm
      assertEquals(elm.nodeType, dom.Node.DOCUMENT_FRAGMENT_NODE)
      assertEquals(elm.textContent, "fragment again")
    }

    test("allows a document fragment as a container") {
      val vnode0 = dom.document.createDocumentFragment()
      val vnode1 = fragment(
        List("I", "am", "a", h("span", List(VNode.text("fragment"))))
      )
      val vnode2 = h("div", "I am an element")

      val vnode1p = patch(vnode0, vnode1)
      var elm = vnode1p.elm
      assertEquals(elm.nodeType, dom.Node.DOCUMENT_FRAGMENT_NODE)

      elm = patch(vnode1p, vnode2).elm
      assertEquals(elm.asInstanceOf[dom.Element].tagName, "DIV")
    }
  }

  group("element hooks") {
    vnode0.test(
      "calls `create` listener before inserted into parent but after children"
    ) { vnode0 =>
      val result = List.newBuilder[PatchedVNode]
      val cb: CreateHook = vnode => {
        assert(vnode.elm.isInstanceOf[dom.Element])
        assertEquals(vnode.elm.childNodes.length, 2)
        assertEquals(vnode.elm.parentNode, null)
        result.addOne(vnode)
        vnode
      }
      val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(create = Some(cb)))),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          ),
          h("span", "Can't touch me")
        )
      )
      patch(vnode0, vnode1)
      assertEquals(result.result().length, 1)
    }

    vnode0.test(
      "calls `insert` listener after both parents, siblings and children have been inserted"
    ) { vnode0 =>
      val result = List.newBuilder[PatchedVNode]
      val cb: InsertHook = (vnode) => {
        assert(vnode.elm.isInstanceOf[dom.Element])
        assertEquals(vnode.elm.childNodes.length, 2)
        assertEquals(
          vnode.elm.parentNode.childNodes.length,
          3
        )
        result.addOne(vnode)
      }
      val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(insert = Some(cb)))),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          ),
          h("span", "Can touch me")
        )
      )
      patch(vnode0, vnode1)
      assertEquals(result.result().length, 1)
    }

    vnode0.test("calls `prepatch` listener") { vnode0 =>
      val result = List.newBuilder[VNode]
      lazy val cb: PrePatchHook = (oldVnode, vnode) => {
        assertEquals(vnode1.children.tail.head, oldVnode.toVNode)
        assertEquals(vnode2.children.tail.head, vnode)
        result.addOne(vnode)
        vnode
      }
      lazy val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(prepatch = Some(cb)))),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          )
        )
      )
      lazy val vnode2 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(prepatch = Some(cb)))),
            List(
              h("span", "Child 1"),
              h("span", "Child 2"),
              h("span", "Child 3")
            )
          )
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(result.result().length, 1)
    }

    vnode0.test("calls `postpatch` after `prepatch` listener") { vnode0 =>
      var pre = 0
      var post = 0
      def preCb() = {
        pre += 1
      }
      def postCb() = {
        assertEquals(pre, post + 1)
        post += 1
      }
      val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook =
              Some(
                Hooks(
                  prepatch = Some((_, vnode) => { preCb(); vnode }),
                  postpatch = Some((_, vnode) => { postCb(); vnode })
                )
              )
            ),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          )
        )
      )
      val vnode2 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook =
              Some(
                Hooks(
                  prepatch = Some((_, vnode) => { preCb(); vnode }),
                  postpatch = Some((_, vnode) => { postCb(); vnode })
                )
              )
            ),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          )
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(pre, 1)
      assertEquals(post, 1)
    }

    vnode0.test("calls `update` listener") { vnode0 =>
      val result1 = ListBuffer.empty[VNode]
      val result2 = ListBuffer.empty[VNode]
      def cb(
          result: ListBuffer[VNode],
          oldVnode: PatchedVNode,
          vnode: VNode
      ) = {
        if (result.length > 0) {
          assertEquals(result(result.length - 1), oldVnode.toVNode)
        }
        result.addOne(vnode)
        vnode
      }
      val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(update = Some(cb(result1, _, _))))),
            List(
              h("span", "Child 1"),
              h(
                "span",
                VNodeData(hook = Some(Hooks(update = Some(cb(result2, _, _))))),
                "Child 2"
              )
            )
          )
        )
      )
      val vnode2 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(update = Some(cb(result1, _, _))))),
            List(
              h("span", "Child 1"),
              h(
                "span",
                VNodeData(hook = Some(Hooks(update = Some(cb(result2, _, _))))),
                "Child 2"
              )
            )
          )
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(result1.length, 1)
      assertEquals(result2.length, 1)
    }

    vnode0.test("calls `remove` listener") { vnode0 =>
      val result = List.newBuilder[VNode]
      val cb: RemoveHook = (vnode, rm) => {
        val parent = vnode.elm.parentNode
        assert(vnode.elm.isInstanceOf[dom.Element])
        assertEquals(vnode.elm.childNodes.length, 2)
        assertEquals(parent.childNodes.length, 2)
        result.addOne(vnode.toVNode)
        rm()
        assertEquals(parent.childNodes.length, 1)
      }
      val vnode1 = h(
        "div",
        List(
          h("span", "First sibling"),
          h(
            "div",
            VNodeData(hook = Some(Hooks(remove = Some(cb)))),
            List(
              h("span", "Child 1"),
              h("span", "Child 2")
            )
          )
        )
      )
      val vnode2 = h("div", List(h("span", "First sibling")))
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(result.result().length, 1)
    }

    vnode0.test(
      "calls `destroy` listener when patching text node over node with children"
    ) { vnode0 =>
      var calls = 0
      def cb() = {
        calls += 1
      }
      val vnode1 = h(
        "div",
        List(
          h(
            "div",
            VNodeData(hook = Some(Hooks(destroy = Some(_ => cb())))),
            List(h("span", "Child 1"))
          )
        )
      )
      val vnode2 = h("div", "Text node")
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(calls, 1)
    }

    vnode0.test("calls `init` and `prepatch` listeners on root") { vnode0 =>
      var count = 0
      var vnode1: VNode = null
      var vnode2: VNode = null
      val init: InitHook = (vnode) => {
        assertEquals(vnode, vnode2)
        count += 1
        vnode
      }
      val prepatch: PrePatchHook = (_, vnode) => {
        assertEquals(vnode, vnode1)
        count += 1
        vnode
      }
      vnode1 = h(
        "div",
        VNodeData(hook =
          Some(Hooks(init = Some(init), prepatch = Some(prepatch)))
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      assertEquals(count, 1)
      vnode2 = h(
        "span",
        VNodeData(hook =
          Some(Hooks(init = Some(init), prepatch = Some(prepatch)))
        )
      )
      patch(vnode1p, vnode2)
      assertEquals(count, 2)
    }

    vnode0.test("removes element when all remove listeners are done") {
      vnode0 =>
        var rm1, rm2, rm3: () => Unit = null
        val patch = init(
          Seq(
            Module(remove = Some((_, rm) => rm1 = rm)),
            Module(remove = Some((_, rm) => rm2 = rm))
          )
        )
        val vnode1 = h(
          "div",
          List(
            h(
              "a",
              VNodeData(hook = Some(Hooks(remove = Some((_, rm) => rm3 = rm))))
            )
          )
        )
        val vnode2 = h("div", List.empty[VNode])
        val vnode1p = patch(vnode0, vnode1)
        var elm = vnode1p.elm
        assertEquals(elm.childNodes.length, 1)
        elm = patch(vnode1p, vnode2).elm
        assertEquals(elm.childNodes.length, 1)
        rm1()
        assertEquals(elm.childNodes.length, 1)
        rm3()
        assertEquals(elm.childNodes.length, 1)
        rm2()
        assertEquals(elm.childNodes.length, 0)
    }

    test("invokes remove hook on replaced root") {
      val result = List.newBuilder[VNode]
      val parent = dom.document.createElement("div")
      val vnode0 = dom.document.createElement("div")
      parent.appendChild(vnode0)
      val cb: RemoveHook = (vnode, rm) => {
        result.addOne(vnode.toVNode)
        rm()
      }
      val vnode1 = h(
        "div",
        VNodeData(hook = Some(Hooks(remove = Some(cb)))),
        List(
          h("b", "Child 1"),
          h("i", "Child 2")
        )
      )
      val vnode2 = h("span", List(h("b", "Child 1"), h("i", "Child 2")))
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(result.result().length, 1)
    }
  }

  group("module hooks") {
    vnode0.test("invokes `pre` and `post` hook") { vnode0 =>
      val result = List.newBuilder[String]
      val patch = init(
        Seq(
          Module(
            pre = Some(() => result.addOne("pre")),
            post = Some(() => result.addOne("post"))
          )
        )
      )
      val vnode1 = h("div")
      patch(vnode0, vnode1)
      assertEquals(result.result(), List("pre", "post"))
    }

    vnode0.test("invokes global `destroy` hook for all removed children") {
      vnode0 =>
        val result = List.newBuilder[VNode]
        val cb: DestroyHook = (vnode) => {
          result.addOne(vnode.toVNode)
        }
        val vnode1 = h(
          "div",
          List(
            h("span", "First sibling"),
            h(
              "div",
              List(
                h(
                  "span",
                  VNodeData(hook = Some(Hooks(destroy = Some(cb)))),
                  "Child 1"
                ),
                h("span", "Child 2")
              )
            )
          )
        )
        val vnode2 = h("div")
        val vnode1p = patch(vnode0, vnode1)
        patch(vnode1p, vnode2)
        assertEquals(result.result().size, 1);
    }

    vnode0.test("handles text vnodes with `undefined` `data` property") {
      vnode0 =>
        val vnode1 = h("div", List(VNode.text(" ")))
        val vnode2 = h("div", List.empty[VNode])
        val vnode1p = patch(vnode0, vnode1)
        patch(vnode1p, vnode2)
    }

    vnode0.test("invokes `destroy` module hook for all removed children") {
      vnode0 =>
        var created = 0;
        var destroyed = 0;
        val patch = init(
          Seq(
            Module(
              create = Some(vnode => { created += 1; vnode }),
              destroy = Some(_ => destroyed += 1)
            )
          )
        )
        val vnode1 = h(
          "div",
          List(
            h("span", "First sibling"),
            h("div", List(h("span", "Child 1"), h("span", "Child 2")))
          )
        )
        val vnode2 = h("div")
        val vnode1p = patch(vnode0, vnode1)
        patch(vnode1p, vnode2)
        assertEquals(created, 4)
        assertEquals(destroyed, 4)
    }

    vnode0.test(
      "does not invoke `create` and `remove` module hook for text nodes"
    ) { vnode0 =>
      var created = 0
      var removed = 0
      val patch = init(
        Seq(
          Module(
            create = Some(vnode => { created += 1; vnode }),
            remove = Some((_, _) => removed += 1)
          )
        )
      )
      val vnode1 = h(
        "div",
        List(
          h("span", "First child"),
          VNode.text(""),
          h("span", "Third child")
        )
      )
      val vnode2 = h("div")
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(created, 2)
      assertEquals(removed, 2)
    }

    vnode0.test("does not invoke `destroy` module hook for text nodes") {
      vnode0 =>
        var created = 0
        var destroyed = 0
        val patch = init(
          Seq(
            Module(
              create = Some(vnode => { created += 1; vnode }),
              destroy = Some(_ => destroyed += 1)
            )
          )
        )
        val vnode1 = h(
          "div",
          List(
            h("span", "First sibling"),
            h(
              "div",
              List(
                h("span", "Child 1"),
                h("span", List(VNode.text("Text 1"), VNode.text("Text 2")))
              )
            )
          )
        )
        val vnode2 = h("div")
        val vnode1p = patch(vnode0, vnode1)
        patch(vnode1p, vnode2)
        assertEquals(created, 4)
        assertEquals(destroyed, 4)
    }
  }

  group("short circuiting") {
    vnode0.test("does not update strictly equal vnodes") { vnode0 =>
      val result = List.newBuilder[VNode]
      val cb: UpdateHook = (vnode, newVNode) => {
        result += vnode.toVNode
        newVNode
      }
      val vnode1 = h(
        "div",
        List(
          h(
            "span",
            VNodeData(hook = Some(Hooks(update = Some(cb)))),
            "Hello"
          ),
          h("span", "there")
        )
      )
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode1)
      assertEquals(result.result().size, 0)
    }

    vnode0.test("does not update strictly equal children") { vnode0 =>
      val result = List.newBuilder[VNode]
      val cb: UpdateHook = (vnode, newVNode) => {
        result += vnode.toVNode
        newVNode
      }
      val vnode1 = h(
        "div",
        List(
          h(
            "span",
            VNodeData(hook = Some(Hooks(update = Some(cb)))),
            "Hello"
          ),
          h("span", "there")
        )
      )
      val vnode2 = h("div").copy(children = vnode1.children)
      val vnode1p = patch(vnode0, vnode1)
      patch(vnode1p, vnode2)
      assertEquals(result.result().size, 0)
    }
  }

  group("patching of random sequence of vnodes") {

    val nodesGen = for {
      n <- Gen.choose(2, 10)
      vnodes <- Gen.listOfN(n, genVNode)
    } yield vnodes

    test("results in correct innerHTML") {

      // for reproducability, we want deterministic tests
      scala.util.Random.setSeed(0)
      keyRng.setSeed(0)

      for {
        _ <- 0 until 1000
        vnodes <- nodesGen.sample
      } {

        // println(s"iteration: $i")
        // println(s"# patches: ${vnodes.length}")
        // println(s"vnode sizes: ${vnodes.map(vnode => size(vnode))}")
        // println(keys(vnodes.head))
        // println(keys(vnodes.tail.head))
        // println()

        val elm = dom.document.createElement("div")
        val vnode = vnodes.tail.foldLeft(patch(elm, vnodes.head)) {
          case (pvnode, vnode) => patch(pvnode, vnode)
        }

        val refElm = patch(dom.document.createElement("div"), vnodes.last).elm
          .asInstanceOf[dom.Element]

        // if (vnode.elm.asInstanceOf[dom.Element].innerHTML != refElm.innerHTML) {
        //  println(vnode.elm.asInstanceOf[dom.Element].innerHTML)
        //  println("-----------------------------------------")
        //  println(refElm.innerHTML)
        // }

        // NOTE: Comparing `innerHTML` only works in the absence of
        // classes b/c `class=""` is semantically the same as not
        // having a `class` attribute at all.
        assertEquals(
          vnode.elm.asInstanceOf[dom.Element].innerHTML,
          refElm.innerHTML
        )

      }
    }

  }

}

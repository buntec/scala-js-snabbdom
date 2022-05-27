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

class ThunkSuite extends BaseSuite {

  val vnode0 = FunFixture[dom.Element](
    setup = { _ =>
      dom.document.createElement("div")
    },
    teardown = { _ => () }
  )

  val patch = init(Seq())

  test("returns vnode with data and render function") {

    val numberInSpan = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      h("span", s"Numbe is ${n}")
    }
    val vnode = thunk("span", "num", numberInSpan, Seq(22))
    assertEquals(vnode.sel, Some("span"))
    assertEquals(vnode.data.key, Some("num"))
    assertEquals(vnode.data.args, Some(Seq(22)))

  }

  vnode0.test("calls render function once on data change") { vnode0 =>
    var called = 0
    val numberInSpan = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode2 = h("div", List(thunk("span", "num", numberInSpan, Seq(2))))
    val vnode1p = patch(vnode0, vnode1)
    assertEquals(called, 1)
    patch(vnode1p, vnode2)
    assertEquals(called, 2)
  }

  vnode0.test("does not call render function when data is unchanged") {
    vnode0 =>
      var called = 0
      // important: we need a stable render function reference!
      val numberInSpan = (arr: Seq[Any]) => {
        called += 1
        val n = arr(0).asInstanceOf[Int]
        h("span", VNodeData(key = Some("num")), s"Number is ${n}")
      }
      val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
      val vnode2 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
      val vnode1p = patch(vnode0, vnode1)
      assertEquals(called, 1)
      patch(vnode1p, vnode2)
      assertEquals(called, 1)
  }

  vnode0.test("calls render function once on data-length change") { vnode0 =>
    var called = 0
    val numberInSpan = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode2 = h("div", List(thunk("span", "num", numberInSpan, Seq(1, 2))))
    val vnode1p = patch(vnode0, vnode1)
    assertEquals(called, 1)
    patch(vnode1p, vnode2)
    assertEquals(called, 2)
  }

  vnode0.test("calls render function once on function change") { vnode0 =>
    var called = 0
    val numberInSpan = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val numberInSpan2 = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode2 = h("div", List(thunk("span", "num", numberInSpan2, Seq(1))))
    val vnode1p = patch(vnode0, vnode1)
    assertEquals(called, 1)
    patch(vnode1p, vnode2)
    assertEquals(called, 2)
  }

  vnode0.test("renders correctly") { vnode0 =>
    var called = 0
    val numberInSpan = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode2 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode3 = h("div", List(thunk("span", "num", numberInSpan, Seq(2))))
    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(called, 1)
    assertEquals(
      elm1.firstChild.asInstanceOf[dom.HTMLElement].tagName.toLowerCase,
      "span"
    )
    assertEquals(
      elm1.firstChild.asInstanceOf[dom.HTMLElement].innerHTML,
      "Number is 1"
    )
    val vnode2p = patch(vnode1p, vnode2)
    val elm2 = vnode2p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(
      elm2.firstChild.asInstanceOf[dom.HTMLElement].tagName.toLowerCase,
      "span"
    )
    assertEquals(
      elm2.firstChild.asInstanceOf[dom.HTMLElement].innerHTML,
      "Number is 1"
    )
    assertEquals(called, 1)
    val vnode3p = patch(vnode2p, vnode3)
    val elm3 = vnode3p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(
      elm3.firstChild.asInstanceOf[dom.HTMLElement].tagName.toLowerCase,
      "span"
    )
    assertEquals(
      elm3.firstChild.asInstanceOf[dom.HTMLElement].innerHTML,
      "Number is 2"
    )
    assertEquals(called, 2)
  }

  vnode0.test("supports leaving out the `key` argument") { vnode0 =>
    val vnodeFn = (args: Seq[Any]) => {
      val s = args.head.asInstanceOf[String]
      h("span.number", s"Hello $s")
    }
    val vnode1 = thunk("span.number", vnodeFn, Seq("World!"))
    val elm = patch(vnode0, vnode1).elm
    assertEquals(elm.innerText, "Hello World!")
  }

  vnode0.test("renders correctly when root") { vnode0 =>
    var called = 0
    val numberInSpan = (arr: Seq[Any]) => {
      called += 1
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val vnode1 = thunk("span", "num", numberInSpan, Seq(1))
    val vnode2 = thunk("span", "num", numberInSpan, Seq(1))
    val vnode3 = thunk("span", "num", numberInSpan, Seq(2))

    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(called, 1)
    assertEquals(elm1.tagName.toLowerCase, "span")
    assertEquals(elm1.innerHTML, "Number is 1")

    val vnode2p = patch(vnode1p, vnode2)
    val elm2 = vnode2p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(elm2.tagName.toLowerCase, "span")
    assertEquals(elm2.innerHTML, "Number is 1")
    assertEquals(called, 1)

    val vnode3p = patch(vnode2p, vnode3)
    val elm3 = vnode3p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(elm3.tagName.toLowerCase, "span")
    assertEquals(elm3.innerHTML, "Number is 2")
    assertEquals(called, 2)

  }

  vnode0.test("can be replaced and removed") { vnode0 =>
    val numberInSpan = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }
    val oddEven = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      val prefix = if (n % 2 == 0) "Even" else "Odd"
      h("span", VNodeData(key = Some("foo")), s"${prefix}: ${n}")
    }

    val vnode1 = h("div", List(thunk("span", "num", numberInSpan, Seq(1))))
    val vnode2 = h("div", List(thunk("div", "oddEven", oddEven, Seq(4))))

    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(
      elm1.firstChild.asInstanceOf[dom.HTMLElement].tagName.toLowerCase,
      "span"
    )
    assertEquals(
      elm1.firstChild.asInstanceOf[dom.HTMLElement].innerHTML,
      "Number is 1"
    )

    val vnode2p = patch(vnode1p, vnode2)
    val elm2 = vnode2p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(
      elm2.firstChild.asInstanceOf[dom.HTMLElement].tagName.toLowerCase,
      "div"
    )
    assertEquals(
      elm2.firstChild.asInstanceOf[dom.HTMLElement].innerHTML,
      "Even: 4"
    )

  }

  vnode0.test("can be replaced and removed when root") { vnode0 =>
    val numberInSpan = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      h("span", VNodeData(key = Some("num")), s"Number is ${n}")
    }

    val oddEven = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      val prefix = if (n % 2 == 0) "Even" else "Odd"
      h("span", VNodeData(key = Some("foo")), s"${prefix}: ${n}")
    }

    val vnode1 = thunk("span", "num", numberInSpan, Seq(1))
    val vnode2 = thunk("div", "oddEven", oddEven, Seq(4))

    val vnode1p = patch(vnode0, vnode1)
    val elm1 = vnode1p.elm.asInstanceOf[dom.HTMLElement]
    assertEquals(elm1.tagName.toLowerCase, "span")
    assertEquals(elm1.innerHTML, "Number is 1")

    val elm2 = patch(vnode1p, vnode2).elm.asInstanceOf[dom.HTMLElement]
    assertEquals(elm2.tagName.toLowerCase, "div")
    assertEquals(elm2.innerHTML, "Even: 4")

  }

  vnode0.test("invokes destroy hook on thunks") { vnode0 =>
    var called = 0
    val destroyHook: DestroyHook = (_: PatchedVNode) => { called += 1 }
    val numberInSpan = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      h(
        "span",
        VNodeData(
          key = Some("num"),
          hook = Some(Hooks(destroy = Some(destroyHook)))
        ),
        s"Number is ${n}"
      )
    }

    val vnode1 = h(
      "div",
      List(
        h("div", "Foo"),
        thunk("span", "num", numberInSpan, Seq(1)),
        h("div", "Foo")
      )
    )
    val vnode2 = h("div", List(h("div", "Foo"), h("div", "Foo")))
    val vnode1p = patch(vnode0, vnode1)
    patch(vnode1p, vnode2)
    assertEquals(called, 1)
  }

  vnode0.test("invokes remove hook on thunks") { vnode0 =>
    var called = 0
    val destroyHook: RemoveHook = (_: PatchedVNode, _: () => Unit) => {
      called += 1
    }
    val numberInSpan = (arr: Seq[Any]) => {
      val n = arr(0).asInstanceOf[Int]
      h(
        "span",
        VNodeData(
          key = Some("num"),
          hook = Some(Hooks(remove = Some(destroyHook)))
        ),
        s"Number is ${n}"
      )
    }

    val vnode1 = h(
      "div",
      List(
        h("div", "Foo"),
        thunk("span", "num", numberInSpan, Seq(1)),
        h("div", "Foo")
      )
    )
    val vnode2 = h("div", List(h("div", "Foo"), h("div", "Foo")))
    val vnode1p = patch(vnode0, vnode1)
    patch(vnode1p, vnode2)
    assertEquals(called, 1)
  }

}

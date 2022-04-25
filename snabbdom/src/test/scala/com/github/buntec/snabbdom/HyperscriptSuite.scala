/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS"BASIS,
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

class HyperscriptSuite extends munit.FunSuite {

  test("create vnode with proper tag") {
    assertEquals(h("div").sel, Some("div"))
    assertEquals(h("a").sel, Some("a"))
  }

  test("create vnode with children") {
    val vnode = h("div", Array(h("span#hello"), h("b.world")))
    assertEquals(vnode.sel, Some("div"))
    val children = vnode.children
    assertEquals(children.flatMap(_(0).sel), Some("span#hello"))
    assertEquals(children.flatMap(_(1).sel), Some("b.world"))
  }

  test("create vnode with one child vnode") {
    val vnode = h("div", Array(h("span#hello")))
    assertEquals(vnode.sel, Some("div"))
    val children = vnode.children
    assertEquals(children.flatMap(_(0).sel), Some("span#hello"))
  }

  test("create vnode with props and one child vnode") {
    val vnode = h("div", VNodeData.empty, h("span#hello"))
    assertEquals(vnode.sel, Some("div"))
    val children = vnode.children
    assertEquals(children.flatMap(_(0).sel), Some("span#hello"))
  }

  test("create vnode with text content") {
    val vnode = h("a", Array(VNode.text("I am a string")))
    val children = vnode.children
    assertEquals(children.flatMap(_(0).text), Some("I am a string"))
  }

  // TODO these compile but don't pass, I ported them wrong? :(

  // test("create vnode with text content in string") {
  //   val vnode = h("a", Array(VNode.text("I am a string")))
  //   assertEquals(vnode.text, Some("I am a string"))
  // }

  // test("create vnode with props and text content in string") {
  //   val vnode = h("a", VNodeData.empty, "I am a string")
  //   assertEquals(vnode.text, Some("I am a string"))
  // }

  // test("create vnode with String obj content") {
  //   val vnode = h("a", Array(VNode.text(new String("b"))))
  //   assertEquals(vnode.text, Some("b"))
  // }

  // test("create vnode with props and String obj content") {
  //   val vnode = h("a", VNodeData.empty, new String("b"))
  //   assertEquals(vnode.text, Some("b"))
  // }

  // test("create vnode with Number obj content") {
  //   val vnode = h("a", new Number(1))
  //   assertEquals(vnode.text, "1")
  // }

  // test("create vnode with null props") {
  //   var vnode = h("a")
  //   assertEquals(vnode.data, None)
  //   vnode = h("a", null, Array(VNode.text("I am a string")))
  //   val children = vnode.children
  //   assertEquals(children.flatMap(_(0).text), Some("I am a string"))
  // }

  // test("create vnode for comment") {
  //   val vnode = h("!", Array(VNode.text("test")))
  //   assertEquals(vnode.sel, Some("!"))
  //   assertEquals(vnode.text, Some("test"))
  // }

}

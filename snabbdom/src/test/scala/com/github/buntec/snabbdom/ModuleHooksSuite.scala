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

class ModuleHooksSuite extends BaseSuite {

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
        result.addOne(vnode)
      }
      val vnode1 = h(
        "div",
        Array(
          h("span", "First sibling"),
          h(
            "div",
            Array(
              h(
                "span",
                VNodeData.builder.withHook(Hooks(destroy = Some(cb))).build,
                "Child 1"
              ),
              h("span", "Child 2")
            )
          )
        )
      )
      val vnode2 = h("div")
      patch(vnode0, vnode1)
      patch(vnode1, vnode2)
      println(result.result())
      assertEquals(result.result().size, 1);
  }

  vnode0.test("handles text vnodes with `undefined` `data` property") {
    vnode0 =>
      val vnode1 = h("div", Array(VNode.text(" ")))
      val vnode2 = h("div", Array.empty[VNode])
      patch(vnode0, vnode1)
      patch(vnode1, vnode2)
  }

  vnode0.test("invokes `destroy` module hook for all removed children") {
    vnode0 =>
      var created = 0;
      var destroyed = 0;
      val patch = init(
        Seq(
          Module(
            create = Some((_, _) => created += 1),
            destroy = Some(_ => destroyed += 1)
          )
        )
      )
      val vnode1 = h(
        "div",
        Array(
          h("span", "First sibling"),
          h("div", Array(h("span", "Child 1"), h("span", "Child 2")))
        )
      )
      val vnode2 = h("div")
      patch(vnode0, vnode1)
      patch(vnode1, vnode2)
      println((created, destroyed)) // TODO prints (5,0)
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
          create = Some((_, _) => created += 1),
          remove = Some((_, _) => removed += 1)
        )
      )
    )
    val vnode1 = h(
      "div",
      Array(
        h("span", "First child"),
        VNode.text(""),
        h("span", "Third child")
      )
    )
    val vnode2 = h("div")
    patch(vnode0, vnode1)
    patch(vnode1, vnode2)
    println((created, removed)) // TODO prints (3,2)
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
            create = Some((_, _) => created += 1),
            destroy = Some(_ => destroyed += 1)
          )
        )
      )
      val vnode1 = h(
        "div",
        Array(
          h("span", "First sibling"),
          h(
            "div",
            Array(
              h("span", "Child 1"),
              h("span", Array(VNode.text("Text 1"), VNode.text("Text 2")))
            )
          )
        )
      )
      val vnode2 = h("div")
      patch(vnode0, vnode1)
      patch(vnode1, vnode2)
      println((created, destroyed)) // TODO prints (5,0)
      assertEquals(created, 4)
      assertEquals(destroyed, 4)
  }
}

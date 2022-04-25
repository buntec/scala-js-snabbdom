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

package com.github.buntec.snabbdom.examples

import org.scalajs.dom

import com.github.buntec.snabbdom._
import com.github.buntec.snabbdom.modules._

object Example {

  def apply(): Unit = {

    val patch = init(
      Seq(
        Attributes.module,
        Classes.module,
        Props.module,
        Styles.module,
        EventListeners.module,
        Dataset.module
      )
    )

    val container = dom.document.getElementById("app");

    val vnode = h(
      "div",
      VNodeData.builder
        .withOn("click" -> ((_: dom.Event) => println("foo")))
        .build,
      Array[VNode](
        h(
          "span",
          VNodeData.builder.withStyle("fontWeight" -> "bold").build,
          "This is bold"
        ),
        " and this is just normal text",
        h(
          "a",
          VNodeData.builder.withProps("href" -> "/foo").build,
          "I'll take you places!"
        )
      )
    )
    // Patch into empty DOM element this modifies the DOM as a side effect
    patch(container, vnode);

    val newVnode = h(
      "div#container.two.classes",
      VNodeData.builder
        .withOn("click" -> ((_: dom.Event) => println("bar")))
        .build,
      Array[VNode](
        h(
          "span",
          VNodeData.builder
            .withStyle("fontWeight" -> "normal", "fontStyle" -> "italic")
            .build,
          "This is now italic type"
        ),
        " and this is still just normal text",
        h(
          "a",
          VNodeData.builder.withProps("href" -> "/foo").build,
          "I'll take you places!"
        )
      )
    )

    // Second `patch` invocation
    patch(vnode, newVnode)

    ()

  }

}

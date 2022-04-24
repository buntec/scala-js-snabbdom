package com.github.buntec.snabbdom.examples

import org.scalajs.dom

import com.github.buntec.snabbdom._
import com.github.buntec.snabbdom.modules._

object HelloWorld {

  def main(args: Array[String]): Unit = {

    val patch = Init(
      Seq(
        // Init patch function with chosen modules
        Attributes.module,
        Classes.module, // makes it easy to toggle classes
        Props.module, // for setting properties on DOM elements
        Styles.module // styleModule, // handles styling on elements with support for animations
        // eventListenersModule, // attaches event listeners
      ),
      None
    )

    val container = dom.document.getElementById("app");

    val data = VNodeData.empty
    val data1 = VNodeData.builder.withStyle("fontWeight" -> "bold").build
    val data2 = VNodeData.builder.withProps("href" -> "/foo").build

    val vnode = h(
      "div",
      data,
      Array(
        h("span", data1, "This is bold"),
        VNode.text(" and this is just normal text"),
        h("a", data2, VNode.text("I'll take you places!"))
      )
    )
    // Patch into empty DOM element this modifies the DOM as a side effect
    patch(container, vnode);

    ()

  }

}

package com.github.buntec.snabbdom.examples

import com.github.buntec.snabbdom.Init
import com.github.buntec.snabbdom.modules.Classes
import com.github.buntec.snabbdom.modules.Props
import com.github.buntec.snabbdom.modules.Attributes

import org.scalajs.dom
import com.github.buntec.snabbdom.h
import com.github.buntec.snabbdom.VNodeData
import com.github.buntec.snabbdom.VNode

object HelloWorld {

  def main(args: Array[String]): Unit = {

    val patch = Init.apply(
      Seq(
        // Init patch function with chosen modules
        Attributes.module,
        Classes.module, // makes it easy to toggle classes
        Props.module // for setting properties on DOM elements
        // styleModule, // handles styling on elements with support for animations
        // eventListenersModule, // attaches event listeners
      ),
      None
    )

    val container = dom.document.getElementById("#app");

    val data = VNodeData.empty
    val data1 = VNodeData.empty
    val data2 = VNodeData.empty

    val vnode = h(
      "div",
      data,
      Array(
        h("span", data1, VNode.text("This is bold")),
        VNode.text(" and this is just normal text"),
        h("a", data2, VNode.text("I'll take you places!"))
      )
    )
    // Patch into empty DOM element this modifies the DOM as a side effect
    patch(container, vnode);

    println("Hello world!")
  }

}

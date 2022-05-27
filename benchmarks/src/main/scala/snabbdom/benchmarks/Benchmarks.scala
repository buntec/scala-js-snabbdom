package snabbdom.benchmarks

import org.scalajs.dom

import snabbdom._
import snabbdom.modules._

import scalajs.js.annotation._

@JSExportTopLevel("SnabbdomBenchmarks")
object Benchmarks {

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

  @JSExport
  def benchmark1(container: dom.Element): Unit = {

    val vnode0p = patch(container, h("div"))

    val vnode1 = h("div", List(h("span", "1"), h("span", "2"), h("span", "3")))
    val vnode2 = h("div", List(h("span", "2"), h("span", "3")))
    val vnode3 = h("div", List(h("span", "3")))
    val vnode4 = h("div", List(h("span", "2"), h("span", "3")))
    val vnode5 = h("div", List(h("span", "1"), h("span", "2"), h("span", "3")))
    val vnode6 = h(
      "div",
      List(h("span", "0"), h("span", "1"), h("span", "2"), h("span", "3"))
    )
    val vnode7 = h(
      "div",
      List(
        h("span", "0"),
        h("span", "1"),
        h("span", "2"),
        h("span", "3"),
        h("span", "4")
      )
    )

    List(vnode1, vnode2, vnode3, vnode4, vnode5, vnode6, vnode7).foldLeft(
      vnode0p
    ) { case (acc, vnode) => patch(acc, vnode) }

    ()

  }

}

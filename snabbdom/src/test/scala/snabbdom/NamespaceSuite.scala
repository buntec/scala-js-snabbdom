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

import scala.util.Random

// Checks random patches against an independent model of the namespace rules
// documented in `init`:
//
// - A new element is created in the vnode's `ns`, or else in the namespace
//   its DOM parent passes on: SVG except inside `foreignObject`, no namespace
//   inside an element without one, and otherwise the document's default.
// - A matched element is reused unless the vnode, after its prepatch hook,
//   names a namespace the element is not in.
class NamespaceSuite extends BaseSuite {

  val svgNS = "http://www.w3.org/2000/svg"
  val htmlNS = "http://www.w3.org/1999/xhtml"
  val nSamples = 400

  val tags = Vector("a", "g", "div", "svg", "foreignObject", "circle")
  val namespaces: Vector[Option[String]] =
    Vector(None, None, None, Some(svgNS), Some(htmlNS), Some(""))

  // `prepatchNs` is the `ns` a prepatch hook on the vnode sets, if any.
  case class Node(
      tag: String,
      ns: Option[String],
      prepatchNs: Option[Option[String]],
      children: List[Node]
  )

  // The tag of a patched element and its DOM element, before a patch.
  case class Snap(sel: String, elm: dom.Element, children: List[Snap]) {
    def all: List[dom.Element] = elm :: children.flatMap(_.all)
  }

  def pick[A](rng: Random, as: Vector[A]): A = as(rng.nextInt(as.size))

  def genNode(rng: Random, depth: Int): Node = Node(
    pick(rng, tags),
    pick(rng, namespaces),
    if (rng.nextInt(4) == 0) Some(pick(rng, namespaces)) else None,
    if (depth == 0) Nil else List.fill(rng.nextInt(4))(genNode(rng, depth - 1))
  )

  // Keeps tags and shape, so every element has a counterpart to match.
  def mutate(rng: Random, node: Node): Node = node.copy(
    ns = if (rng.nextInt(3) == 0) pick(rng, namespaces) else node.ns,
    prepatchNs =
      if (rng.nextInt(3) != 0) node.prepatchNs
      else if (rng.nextBoolean()) None
      else Some(pick(rng, namespaces)),
    children = node.children.map(mutate(rng, _))
  )

  def withNs(vnode: VNode, ns: Option[String]): VNode = vnode match {
    case e: VNode.Element => e.copy(data = e.data.copy(ns = ns))
    case other            => other
  }

  def build(node: Node): VNode.Element = {
    val hook = node.prepatchNs.map { ns =>
      Hooks(prepatch = Some((_: PatchedVNode, v: VNode) => withNs(v, ns)))
    }
    h(node.tag, VNodeData(ns = node.ns, hook = hook), node.children.map(build))
  }

  def elements(node: dom.Node): List[dom.Element] =
    (0 until node.childNodes.length)
      .map(node.childNodes(_))
      .filter(_.nodeType == 1)
      .map(_.asInstanceOf[dom.Element])
      .toList

  def snap(vnode: VNode.Element, elm: dom.Element): Snap =
    Snap(
      vnode.sel,
      elm,
      vnode.children
        .collect { case c: VNode.Element => c }
        .zip(elements(elm))
        .map { case (c, e) =>
          snap(c, e)
        }
    )

  def uri(ns: String): String = if (ns.isEmpty) null else ns

  def context(parent: dom.Node, default: String): String =
    if (parent == null || parent.nodeType != 1) default
    else {
      val p = parent.asInstanceOf[dom.Element]
      p.namespaceURI match {
        case `htmlNS`                                  => default
        case `svgNS` if p.localName == "foreignObject" => default
        case ns                                        => ns
      }
    }

  def check(
      vnode: VNode.Element,
      elm: dom.Element,
      old: Option[Snap],
      oldElms: List[dom.Element],
      default: String,
      path: String
  ): Unit = {
    def created(vnode: VNode.Element, elm: dom.Element, path: String): Unit = {
      assert(!oldElms.exists(_ eq elm), s"$path: expected a new element")
      val expected = vnode.data.ns.fold(context(elm.parentNode, default))(uri)
      assertEquals(elm.namespaceURI, expected, s"$path: namespace")
      val es = elements(elm)
      assertEquals(es.size, vnode.children.size, s"$path: children")
      vnode.children.zip(es).zipWithIndex.foreach {
        case ((c: VNode.Element, e), i) => created(c, e, s"$path/$i")
        case _                          => ()
      }
    }
    old match {
      case Some(o) if o.sel == vnode.sel =>
        val post = vnode.data.hook
          .flatMap(_.prepatch)
          .fold[VNode](vnode)(_(null, vnode))
          .asInstanceOf[VNode.Element]
        if (post.data.ns.forall(uri(_) == o.elm.namespaceURI)) {
          assert(elm eq o.elm, s"$path: expected the element to be reused")
          val es = elements(elm)
          assertEquals(es.size, post.children.size, s"$path: children")
          post.children.zip(es).zipWithIndex.foreach {
            case ((c: VNode.Element, e), i) =>
              check(c, e, o.children.lift(i), oldElms, default, s"$path/$i")
            case _ => ()
          }
        } else {
          assert(!(elm eq o.elm), s"$path: expected the element to be replaced")
          created(post, elm, path)
        }
      case _ => created(vnode, elm, path)
    }
  }

  def run(doc: dom.Document, api: Option[DomApi], default: String): Unit = {
    val rng = new Random(0)
    val patch = init(Seq.empty, api)
    (0 until nSamples).foreach { sample =>
      val trees = {
        val t1 = genNode(rng, 3)
        val t2 = mutate(rng, t1)
        List(t1, t2, mutate(rng, t2))
      }
      val parent: dom.Node = rng.nextInt(4) match {
        case 0 => null
        case 1 => doc.createElementNS(htmlNS, "div")
        case 2 => doc.createElementNS(svgNS, "svg")
        case _ => doc.createElementNS(svgNS, "foreignObject")
      }
      val targetTag = if (rng.nextBoolean()) trees.head.tag else pick(rng, tags)
      val target = pick(rng, namespaces) match {
        case None     => doc.createElement(targetTag)
        case Some(ns) => doc.createElementNS(ns, targetTag)
      }
      if (parent != null) { parent.appendChild(target); () }
      val targetSel =
        if (target.namespaceURI == htmlNS) target.tagName.toLowerCase
        else target.tagName

      def clue = s"sample $sample: target <$targetTag> in " +
        s"${target.namespaceURI} under ${Option(parent).map(_.nodeName)}\n" +
        trees.mkString("\n")

      try {
        val first = build(trees.head)
        var patched = patch(target, first)
        check(
          first,
          patched.node.asInstanceOf[dom.Element],
          Some(Snap(targetSel, target, Nil)),
          List(target),
          default,
          "1"
        )
        var prev = first
        trees.tail.zipWithIndex.foreach { case (tree, i) =>
          val old = snap(prev, patched.node.asInstanceOf[dom.Element])
          val next = build(tree)
          patched = patch(patched, next)
          val elm = patched.node.asInstanceOf[dom.Element]
          check(next, elm, Some(old), old.all, default, s"${i + 2}")
          prev = next
        }
      } catch {
        case e: Throwable =>
          throw new AssertionError(s"${e.getMessage}\n$clue", e)
      }
    }
  }

  test("follows the namespace rules in an HTML document") {
    run(dom.document, None, htmlNS)
  }

  test("follows the namespace rules in an XML document") {
    val doc = XmlDomApi.svgDocument()
    run(doc, Some(XmlDomApi(doc)), null)
  }

}

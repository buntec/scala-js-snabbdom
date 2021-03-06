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

package snabbdom

import org.scalacheck.Gen
import org.scalajs.dom
import snabbdom.modules._

import scala.concurrent.duration._
import scala.concurrent.Future

class RandomizedSuite extends BaseSuite {

  // generous timeout for scalacheck-based tests with large number of samples
  override val munitTimeout = 10.minutes

  val nSamples = 1000

  val patch = init(
    Seq(
      Classes.module,
      Props.module,
      EventListeners.module,
      Styles.module
    )
  )

  group("patching a random sequence of vnodes") {

    test("results in expected innerHTML") {

      // need macrotask EC to allow rendering between async boundaries to avoid rendering timeouts
      import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

      scala.util.Random.setSeed(0)

      // NOTE: Comparing `innerHTML` only works in the absence of
      // classes and styles b/c `class=""` is semantically the same as not
      // having a `class` attribute at all, and similarly for `style=""`.
      val config = VNodeGen.Config(
        keys = true,
        props = true,
        attrs = true,
        classes = false,
        style = false,
        dataset = true
      )

      val vnodeGen = VNodeGen(config)

      val nodesGen = for {
        n <- Gen.choose(2, 5)
        vnodes <- Gen.listOfN(n, vnodeGen.gen)
      } yield vnodes

      Future.sequence {
        (0 until nSamples).map { _ =>
          Future(nodesGen.sample)
            .map {
              case None => Future.unit
              case Some(vnodes) =>
                val elm = dom.document.createElement("div")
                val vnode = vnodes.tail.foldLeft(patch(elm, vnodes.head)) {
                  case (pvnode, vnode) => patch(pvnode, vnode)
                }

                val refElm =
                  patch(dom.document.createElement("div"), vnodes.last).node
                    .asInstanceOf[dom.Element]

                val a = vnode.node.asInstanceOf[dom.Element].innerHTML
                val b = refElm.innerHTML

                if (a != b) {
                  println(vnodes.map(_.prettyPrint).mkString("\n\n"))
                  println()
                  println(a)
                  println()
                  println(b)
                }

                assertEquals(a, b)

            }
        }
      }
    }

    test("results in expected vnode after calling `toVNode`") {

      // need macrotask EC to allow rendering between async boundaries to avoid rendering timeouts
      import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

      scala.util.Random.setSeed(0)

      val config = VNodeGen.Config(
        keys = true,
        props = true,
        attrs = true,
        classes = true,
        style =
          false, // doesn't work here b/c adding and removing a CSS property results in an empty-string attribute value
        dataset = true
      )

      val vnodeGen = VNodeGen(config)

      val nodesGen = for {
        n <- Gen.choose(2, 5)
        vnodes <- Gen.listOfN(n, vnodeGen.gen)
      } yield vnodes

      Future.sequence {
        (0 until nSamples).map { _ =>
          Future(nodesGen.sample).map {
            case None => Future.unit
            case Some(vnodes) =>
              val elm = dom.document.createElement("div")

              val vnode = vnodes.tail.foldLeft(patch(elm, vnodes.head)) {
                case (pvnode, vnode) => patch(pvnode, vnode)
              }

              val refElm =
                patch(dom.document.createElement("div"), vnodes.last).node

              val v1 = toVNode(vnode.node).toVNode
              val v2 = toVNode(refElm).toVNode

              if (v1 != v2) {
                println(vnodes.map(_.prettyPrint).mkString("\n\n"))
                println()
                println(s"v1: ${v1.prettyPrint}")
                println()
                println(s"v2: ${v2.prettyPrint}")
              }

              assertEquals(v1, v2)

          }
        }
      }
    }

  }

}

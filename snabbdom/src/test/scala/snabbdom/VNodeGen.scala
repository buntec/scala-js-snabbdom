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
import org.scalacheck.Gen.lzy
import org.scalacheck.rng.Seed

trait VNodeGen {

  def gen: Gen[VNode]

}

object VNodeGen {

  final case class Config(
      keys: Boolean,
      props: Boolean,
      attrs: Boolean,
      classes: Boolean,
      style: Boolean,
      dataset: Boolean
  )

  def apply(config: Config): VNodeGen = {

    // Used for data (keys, props, classes, etc.) generation
    // to ensure that the same sequence of keys, props, etc.
    // is used for successive vnodes.
    val dataRng = new scala.util.Random(0)

    val genClasses: Gen[Map[String, Boolean]] = Gen.choose(0, 3).flatMap { n =>
      Gen.mapOfN(
        n,
        for {
          n <- Gen.choose(4, 8)
          name <- Gen.stringOfN(n, Gen.alphaChar)
          value <- Gen.oneOf(true, false)
        } yield (name, value)
      )
    }

    val genProps: Gen[Map[String, String]] = Gen.choose(0, 3).flatMap { n =>
      Gen.mapOfN(
        n,
        for {
          n <- Gen.choose(4, 8)
          name <- Gen.stringOfN(n, Gen.alphaChar)
          k <- Gen.choose(4, 8)
          value <- Gen.stringOfN(k, Gen.alphaChar)
        } yield (name, value)
      )
    }

    val genAttrs = genProps
    val genDataset = genProps

    val cssProps = Map(
      "fontWeight" -> List("normal", "bold"),
      "background" -> List("green", "red"),
      "border" -> List("solid", "2px dotted", "medium dashed green"),
      "padding" -> List("1em", "5% 10%", "1em 2em 2em", "5px 1em 0 2em"),
      // some CSS custom properties
      "--first-color" -> List("#16f", "#ff7", "#290"),
      "--second-color" -> List("#16f", "#ff7", "#290")
    )

    val genStyle: Gen[Map[String, String]] = Gen.choose(0, 3).flatMap { n =>
      Gen.mapOfN(
        n,
        Gen.oneOf(cssProps).flatMap { case (key, values) =>
          Gen.oneOf(values).map(key -> _)
        }
      )
    }

    def genVNodeData(implicit config: VNodeGen.Config): Gen[VNodeData] = {
      // dirty hack to allow temporary switching of rngs
      var globalSeed: Seed = null
      for {
        _ <- Gen.const(()).withPerturb { seed =>
          globalSeed = seed
          Seed(dataRng.nextLong()) // use seed driven by data rng
        }
        key <-
          if (config.keys)
            Gen
              .frequency(
                (1 -> Gen.const(None)),
                (1 -> Gen.some(Gen.uuid.map(_.toString.take(8))))
              )
          else Gen.const(None)
        props <-
          if (config.props) genProps else Gen.const(Map.empty[String, String])
        attrs <-
          if (config.attrs) genAttrs else Gen.const(Map.empty[String, String])
        classes <-
          if (config.classes) genClasses
          else Gen.const(Map.empty[String, Boolean])
        style <-
          if (config.style) genStyle else Gen.const(Map.empty[String, String])
        dataset <-
          if (config.dataset) genDataset
          else Gen.const(Map.empty[String, String])
        _ <- Gen.const(()).withPerturb(_ => globalSeed) // back to global seed
      } yield {
        VNodeData(
          key = key,
          attrs = attrs.map { case (key, value) =>
            key -> AttrValue.StringAttrValue(value)
          },
          props = props,
          classes = classes,
          style = style,
          dataset = dataset
        )
      }
    }

    val flowContent =
      Set(
        "div",
        "a",
        "h1",
        "h2",
        "h3",
        "h4",
        "span",
        "select",
        "button",
        "input"
      )
    val phrasingContent = Set("span", "button", "input", "cite")
    val allContent = flowContent ++ phrasingContent
    val contentModel = Map(
      "div" -> flowContent,
      "a" -> allContent,
      "h1" -> phrasingContent,
      "h2" -> phrasingContent,
      "h3" -> phrasingContent,
      "h4" -> phrasingContent,
      "span" -> phrasingContent,
      "button" -> phrasingContent,
      "cite" -> phrasingContent,
      "select" -> Set("option"),
      "input" -> Set.empty[String],
      "option" -> Set.empty[String]
    )

    def genLeaf(
        tags: Set[String]
    )(implicit config: VNodeGen.Config): Gen[VNode] =
      for {
        n <- Gen.choose(3, 20)
        tag <- Gen.oneOf(tags)
        text <- Gen.stringOfN(n, Gen.alphaChar)
        data <- genVNodeData
      } yield h(tag, data, text)

    def genTree(
        tags: Set[String]
    )(implicit config: VNodeGen.Config): Gen[VNode] =
      for {
        n <- Gen.choose(1, 10)
        tag <- Gen.oneOf(tags)
        childTags = contentModel(tag)
        children <-
          if (childTags.nonEmpty) Gen.listOfN(n, genVNodePre(childTags))
          else Gen.const(Nil)
        data <- genVNodeData
      } yield h(tag, data, children)

    // Dies out fast enough not to explode, but has high odds of being just a leaf.
    // To get a more interesting distribution for the final `Gen[VNode]`, we
    // therefore condition this generator on `size(vnode) > 2`.
    def genVNodePre(tags: Set[String])(implicit
        config: VNodeGen.Config
    ): Gen[VNode] = Gen.frequency(
      (10, genLeaf(tags)),
      (1, lzy(genTree(tags)))
    )

    new VNodeGen {

      // We reset the data rng after every evaluation so that
      // successive vnodes will use the same sequence of keys, props, etc.
      def gen: Gen[VNode] =
        genVNodePre(allContent)(config)
          .flatMap { vnode =>
            Gen.delay { dataRng.setSeed(0); Gen.const(vnode) }
          }
          .retryUntil(vnode => VNodeGen.size(vnode) > 2)
          .map { vnode =>
            // println(s"size=${size(vnode)}, depth=${depth(vnode)}")
            vnode
          }
    }

  }

  private def size(vnode: VNode): Long = vnode match {
    case VNode.Comment(_)              => 1
    case VNode.Text(_)                 => 1
    case VNode.Element(_, _, children) => 1 + children.map(size).sum
  }

  private def keys(vnode: VNode): Set[String] =
    vnode match {
      case VNode.Comment(_) => Set.empty
      case VNode.Element(_, data, children) =>
        data.key.toSet.union(children.map(keys).toList.flatten.toSet)
      case VNode.Text(_) => Set.empty
    }

  private def depth(vnode: VNode): Long =
    vnode match {
      case VNode.Comment(_) => 1
      case VNode.Element(_, _, children) =>
        1 + children.map(depth).maxOption.getOrElse(0L)
      case VNode.Text(_) => 1
    }

}

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

class VNodeData(
    var props: Option[Map[String, PropValue]],
    var attrs: Option[Map[String, AttrValue]],
    var classes: Option[Map[String, ClassValue]],
    var style: Option[Map[String, StyleValue]],
    var dataset: Option[Map[String, String]],
    var on: Option[Map[String, EventHandler]],
    var hook: Option[Hooks],
    var key: Option[String],
    var ns: Option[String], // for SVG
    var fn: Option[Seq[Any] => VNode], // for thunks
    var args: Option[Seq[Any]], // for thunks
    var is: Option[String]
)

object VNodeData {

  def empty =
    new VNodeData(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

  def builder = new Builder()

  class Builder() {
    private val data = empty

    def build: VNodeData = data

    def withKey(key: String): Builder = {
      data.key = Some(key)
      this
    }

    def withProps(props: (String, PropValue)*): Builder = {
      data.props = Some(props.toMap)
      this
    }

    def withAttrs(attrs: (String, AttrValue)*): Builder = {
      data.attrs = Some(attrs.toMap)
      this
    }

    def withClasses(classes: (String, ClassValue)*): Builder = {
      data.classes = Some(classes.toMap)
      this
    }

    def withStyle(style: (String, StyleValue)*): Builder = {
      data.style = Some(style.toMap)
      this
    }

    def withOn(on: (String, EventHandler)*): Builder = {
      data.on = Some(on.toMap)
      this
    }

    def withHook(hook: Hooks): Builder = {
      data.hook = Some(hook)
      this
    }

    def withNs(ns: String): Builder = {
      data.ns = Some(ns)
      this
    }

  }

}

package com.github.buntec

import scala.scalajs.js
import scala.scalajs.js.|

package object snabbdom {

  type Key = String | Double | Int

  type PropValue = Any
  type AttrValue = String | Boolean | Double | Int
  type StyleValue = String | js.Dictionary[String]
  type KeyValue =
    String | Double | Int // https://github.com/snabbdom/snabbdom#key--string--number

}

package com.github.buntec

import scala.scalajs.js
import scala.scalajs.js.|
import scala.collection.mutable

package object snabbdom {

  type VNodeQueue = mutable.ArrayBuffer[VNode]


  type PropValue = Any
  type AttrValue = Any // JS snabbdom uses string | number | boolean
  type ClassValue = Boolean
  type StyleValue = String | js.Dictionary[String]
  type KeyValue =
    String | Double | Int // https://github.com/snabbdom/snabbdom#key--string--number

}

package com.github.buntec

import scala.collection.mutable

package object snabbdom {

  type VNodeQueue = mutable.ArrayBuffer[VNode]

  type PropValue = Any
  type AttrValue = Any // JS snabbdom uses string | number | boolean
  type ClassValue = Boolean
  type StyleValue = String
  type KeyValue = String

}

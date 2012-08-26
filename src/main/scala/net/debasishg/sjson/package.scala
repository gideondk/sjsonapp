package net.debasishg
package sjson

import scalaz._
import Scalaz._

import dispatch.json._
import sjson.json.JsonSerialization._

package object json {
  implicit def JsStringMonoid = new Monoid[JsString] {
    val zero = JsString("")
    def append(js1: JsString, js2: => JsString) = {
      val JsString(s1) = js1
      val JsString(s2) = js2
      JsString(s1 |+| s2)
    }
  }

  implicit def JsNumberMonoid = new Monoid[JsNumber] {
    val zero = JsNumber(0)
    def append(js1: JsNumber, js2: => JsNumber) = {
      val JsNumber(s1) = js1
      val JsNumber(s2) = js2
      JsNumber(s1 + s2)
    }
  }

  implicit def JsArrayMonoid = new Monoid[JsArray] {
    val zero = JsArray(List.empty[JsValue])
    def append(js1: JsArray, js2: => JsArray) = {
      val JsArray(l1) = js1
      val JsArray(l2) = js2
      JsArray(l1 |+| l2)
    }
  }


  implicit def MapJsObjectMonoid = new Monoid[Map[JsString, JsObject]] {
    val zero = Map.empty[JsString, JsObject]
    def append(js1: Map[JsString, JsObject], js2: => Map[JsString, JsObject]) = {
      val m = (js1.map {case (k, JsObject(m)) => (k, m)} |+| js2.map {case (k, JsObject(m)) => (k, m)})
      m.map {case(k, v) => (k, JsObject(v))}
    }
  }

  implicit def JsObjectMonoid = new Monoid[JsObject] {
    val zero = JsObject(Map.empty[JsString, JsValue])
    def append(js1: JsObject, js2: => JsObject) = {
      val JsObject(s1) = js1
      val JsObject(s2) = js2
      JsObject(s1 |+| s2)
    }
  }

  implicit def JsValueMonoid: Monoid[JsValue] = new Monoid[JsValue] {
    val zero = JsObject(Map.empty[JsString, JsValue])      
    def append(js1: JsValue, js2: => JsValue) = (js1, js2) match {
      case (s1: JsString, s2: JsString) => s1 |+| s2
      case (n1: JsNumber, n2: JsNumber) => n1 |+| n2
      case (l1: JsArray, l2: JsArray) => l1 |+| l2
      case (o1: JsObject, o2: JsObject) => o1 |+| o2
      case x => sys.error("Invalid combination: " + x)
    }
  }
}

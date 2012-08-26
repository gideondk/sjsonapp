package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import dispatch.json._

trait Primitives extends Protocol {
  implicit object IntFormat extends Format[Int] {
    def writes(o: Int) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.intValue.success
      case _ => "Int expected".fail.toValidationNel
    }
  }

  implicit object ShortFormat extends Format[Short] {
    def writes(o: Short) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.shortValue.success
      case _ => "Short expected".fail.toValidationNel
    }
  }

  implicit object LongFormat extends Format[Long] {
    def writes(o: Long) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.longValue.success
      case _ => "Long expected".fail.toValidationNel
    }
  }

  implicit object FloatFormat extends Format[Float] {
    def writes(o: Float) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.floatValue.success
      case _ => "Float expected".fail.toValidationNel
    }
  }

  implicit object DoubleFormat extends Format[Double] {
    def writes(o: Double) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.doubleValue.success
      case _ => "Double expected".fail.toValidationNel
    }
  }

  implicit object BooleanFormat extends Format[Boolean] {
    def writes(o: Boolean) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsTrue => true.success
      case JsFalse => false.success
      case _ => "Boolean expected".fail.toValidationNel
    }
  }

  implicit object StringFormat extends Format[String] {
    def writes(o: String) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsString(s) => s.success
      case _ => "String expected".fail.toValidationNel
    }
  }
  implicit object JsValueFormat extends Format[JsValue] {
    def writes(o: JsValue) = o.success
    def reads(json: JsValue) = json.success
  }
}

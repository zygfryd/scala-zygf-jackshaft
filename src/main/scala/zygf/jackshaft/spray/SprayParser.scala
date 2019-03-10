package zygf.jackshaft.spray

import spray.json._
import zygf.jackshaft.impl.OptimizedMapParsingMiddleware

object SprayParser extends OptimizedMapParsingMiddleware[JsValue, Vector[JsValue]]
{
  private val zero = Integer.valueOf(0)
  
  override def buildNull() = JsNull
  
  override def buildTrue() = JsTrue
  
  override def buildFalse() = JsFalse
  
  override def buildArray() = JsArray.empty
  
  override def buildObject() = JsObject.empty
  
  override def buildObject(map: Map[String, JsValue]) =
    JsObject(map)
  
  override def emptyArray() =
    Vector.empty[JsValue]
  
  override def growArray(array: Vector[JsValue], value: JsValue) =
    array :+ value
  
  override def buildArray(array: Vector[JsValue]) =
    JsArray(array)
  
  override def buildNumber(num: Number) = {
    num match {
      case `zero` =>
        JsNumber.zero
      case n: Integer =>
        JsNumber(n)
      case n: java.lang.Long =>
        JsNumber(n)
      case n: java.lang.Double =>
        JsNumber(n)
      case n: java.math.BigInteger =>
        JsNumber(n)
      case n: java.math.BigDecimal =>
        JsNumber(n)
      case _ =>
        ???
    }
  }
  
  override def buildString(text: String) = {
    if (text eq "")
      JsString.empty
    else
      JsString(text)
  }
}

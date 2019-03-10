package zygf.jackshaft.spray

import scala.collection.immutable.VectorBuilder

import spray.json._
import zygf.jackshaft.impl.OptimizedMapParsingMiddleware

object SprayParser extends OptimizedMapParsingMiddleware[JsValue, VectorBuilder[JsValue]]
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
    new VectorBuilder[JsValue]
  
  override def growArray(array: VectorBuilder[JsValue], value: JsValue) =
    array += value
  
  override def buildArray(array: VectorBuilder[JsValue]) =
    JsArray(array.result)
  
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

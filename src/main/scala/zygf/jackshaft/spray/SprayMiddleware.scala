package zygf.jackshaft.spray

import scala.collection.immutable.TreeMap

import spray.json._
import zygf.jackshaft.impl.JacksonMiddleware

final class SprayMiddleware extends JacksonMiddleware[JsValue, Vector[JsValue], Map[String, JsValue]](classOf[JsValue])
{
  import SprayMiddleware._
  
  override protected def jNull() = JsNull
  
  override protected def jTrue() = JsTrue
  
  override protected def jFalse() = JsFalse
  
  override protected def emptyJArray() = JsArray.empty
  
  override protected def emptyJObject() = JsObject.empty
  
  override protected def smallJObject(keys: Array[String], vals: Array[JsValue with Object], at: Int, count: Int): spray.json.JsValue = {
    import scala.collection.immutable.Map._
    
    count match {
      case 1 => JsObject(new Map1(keys(at),     vals(at)))
      case 2 => JsObject(new Map2(keys(at),     vals(at),
                                  keys(at + 1), vals(at + 1)))
      case 3 => JsObject(new Map3(keys(at),     vals(at),
                                  keys(at + 1), vals(at + 1),
                                  keys(at + 2), vals(at + 2)))
      case 4 => JsObject(new Map4(keys(at),     vals(at),
                                  keys(at + 1), vals(at + 1),
                                  keys(at + 2), vals(at + 2),
                                  keys(at + 3), vals(at + 3)))
      case _ =>
        ???
    }
  }
  
  override protected def emptyMap() =
    TreeMap.empty[String, JsValue]
  
  override protected def growMap(map: Map[String, JsValue], key: String, value: JsValue) =
    map.updated(key, value)
  
  override protected def mapToJObject(map: Map[String, JsValue]) =
    JsObject(map)
  
  override protected def emptyArray() =
    Vector.empty[JsValue]
  
  override protected def growArray(array: Vector[JsValue], value: JsValue) =
    array :+ value
  
  override protected def arrayToJArray(array: Vector[JsValue]) =
    JsArray(array)
  
  override protected def numberToJNumber(num: Number) = {
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
  
  override protected def stringToJString(text: String) = {
    if (text eq "")
      JsString.empty
    else
      JsString(text)
  }
}

object SprayMiddleware
{
  private val zero = Integer.valueOf(0)
}

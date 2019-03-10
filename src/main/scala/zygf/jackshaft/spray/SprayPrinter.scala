package zygf.jackshaft.spray

import spray.json._
import zygf.jackshaft.impl.{JsonPrinter, PrintingMiddleware}

object SprayPrinter extends PrintingMiddleware[JsValue]
{
  // emitters are written very defensively, because JSON libraries usually don't
  // check for nulls when ASTs are constructed, and we can't just crash in
  // the middle of sending a HTTP response
  
  def emit(json: JsValue, print: JsonPrinter[JsValue], buffer: Array[Byte], offset: Int): Int = {
    (json: Any) match {
      case null =>
        print.reportError("null value")
        print.printNull(buffer, offset)
      
      case v: JsString =>
        if (v.value ne null) {
          print.printString(buffer, offset, v.value)
        }
        else {
          print.reportError("null string")
          print.printNull(buffer, offset)
        }
      
      case v: JsObject =>
        if (v.fields ne null)
          print.printObject(buffer, offset, v.fields)
        else {
          print.reportError("null object fields")
          print.printNull(buffer, offset)
        }
      
      case v: JsArray =>
        if (v.elements ne null)
          print.printArray(buffer, offset, v.elements)
        else {
          print.reportError("null array elements")
          print.printNull(buffer, offset)
        }
        
      case v: JsNumber =>
        if (v.value ne null) {
          print.printRaw(buffer, offset, v.value.bigDecimal.toString)
        }
        else {
          print.reportError("null number")
          print.printNull(buffer, offset)
        }
      
      case v: JsBoolean =>
        print.printBoolean(buffer, offset, v.value)
      
      case JsNull =>
        print.printNull(buffer, offset)
        
      case other => // perhaps a miscasted object
        print.reportError(s"unrecognized type: ${other.getClass.getName}")
        print.printNull(buffer, offset)
    }
  }
}

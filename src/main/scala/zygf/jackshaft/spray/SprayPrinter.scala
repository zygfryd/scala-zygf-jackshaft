package zygf.jackshaft.spray

import spray.json._
import zygf.jackshaft.impl.{JsonPrinter, PrintingMiddleware}

object SprayPrinter extends PrintingMiddleware[JsValue]
{
  // emitters are written very defensively, because JSON libraries usually don't
  // check for nulls when ASTs are constructed, and we can't just crash in
  // the middle of sending a HTTP response
  
  def emit(json: JsValue, print: JsonPrinter[JsValue]): Unit = {
    (json: Any) match {
      case null =>
        print.reportError("null value")
        print.printNull()
      
      case v: JsString =>
        if (v.value ne null) {
          print.printString(v.value)
        }
        else {
          print.reportError("null string")
          print.printNull()
        }
      
      case v: JsObject =>
        if (v.fields ne null)
          print.printObject(v.fields)
        else {
          print.reportError("null object fields")
          print.printNull()
        }
      
      case v: JsArray =>
        if (v.elements ne null)
          print.printArray(v.elements)
        else {
          print.reportError("null array elements")
          print.printNull()
        }
        
      case v: JsNumber =>
        if (v.value ne null) {
          print.printRaw(v.value.bigDecimal.toString)
        }
        else {
          print.reportError("null number")
          print.printNull()
        }
      
      case v: JsBoolean =>
        print.printBoolean(v.value)
      
      case JsNull =>
        print.printNull()
        
      case other => // perhaps a miscasted object
        print.reportError(s"unrecognized type: ${other.getClass.getName}")
        print.printNull()
    }
  }
}

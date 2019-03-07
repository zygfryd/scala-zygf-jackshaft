package zygf.jackshaft.spray

import com.fasterxml.jackson.core.JsonGenerator
import spray.json._
import zygf.jackshaft.impl.JsonEmitter

class SprayEmitter(input: JsValue) extends JsonEmitter[JsValue](input)
{
  // emitters are written very defensively, because JSON libraries usually don't
  // check for nulls when ASTs are constructed, and we can't just crash in
  // the middle of sending a HTTP response
  
  def emit(json: JsValue, gen: JsonGenerator): Unit = {
    (json: Any) match {
      case null =>
        reportError("null value")
        gen.writeNull()
      
      case v: JsString =>
        if (v.value ne null) {
          gen.writeString(v.value)
        }
        else {
          reportError("null string")
          gen.writeNull()
        }
      
      case v: JsObject =>
        if (v.fields ne null)
          pushObject(v.fields.iterator, gen)
        else {
          reportError("null object fields")
          gen.writeNull()
        }
      
      case v: JsArray =>
        if (v.elements ne null)
          pushArray(v.elements.iterator, gen)
        else {
          reportError("null array elements")
          gen.writeNull()
        }
        
      case v: JsNumber =>
        if (v.value ne null) {
          gen.writeNumber(v.value.bigDecimal)
        }
        else {
          reportError("null number")
          gen.writeNull()
        }
      
      case v: JsBoolean =>
        gen.writeBoolean(v.value)
      
      case JsNull =>
        gen.writeNull()
        
      case other => // perhaps a miscasted object
        reportError(s"unrecognized type: ${other.getClass.getName}")
        gen.writeNull()
    }
  }
}

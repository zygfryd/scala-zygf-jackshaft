package zygf.jackshaft.impl

import scala.collection.AbstractIterator

import com.fasterxml.jackson.core.JsonGenerator

// PROBLEM: jackson doesn't have an API for printing strings incrementally
//          also, this whole thing is ugly and inefficient

abstract class JsonEmitter[J](input: J)
{
  import JsonEmitter._
  
  private val instructions = new java.util.ArrayList[AbstractIterator[Any]]()
  private val path = new java.util.ArrayList[Any]()
  private var _errors = Nil: List[String]
  
  protected def emit(json: J, gen: JsonGenerator): Unit
  
  final def emit(gen: JsonGenerator): Boolean = {
    var json = null: Any
    
    var i = instructions.size - 1
    while (null == json && i >= 0) {
      val top = instructions.get(i)
      if (top.hasNext)
        json = top.next()
      else {
        instructions.remove(i)
        path.remove(i) match {
          case _: String =>
            gen.writeEndObject()
          case i: Integer =>
            if (i > -2)
              gen.writeEndArray()
        }
        i -= 1
      }
    }
    
    json match {
      case null =>
        true
        
      case pair: (String, J) @unchecked =>
        path.set(i, pair._1)
        if (pair._1 ne null) {
          gen.writeFieldName(pair._1)
          emit(pair._2, gen)
        }
        else {
          reportError("null object key")
        }
        false
        
      case other: J @unchecked =>
        path.set(i, path.get(i).asInstanceOf[Integer] + 1)
        emit(other, gen)
        false
    }
  }
  
  private def pushOne(value: J): Unit = {
    instructions.add(Iterator.single(value).asInstanceOf[AbstractIterator[Any]])
    path.add(-3)
  }
  
  final protected def pushArray(values: Iterator[J], gen: JsonGenerator): Unit = {
    gen.writeStartArray()
    values match {
      case it: AbstractIterator[Any] =>
        instructions.add(it)
      case it: Iterator[Any] =>
        instructions.add(new WrappedIterator(it))
    }
    path.add(-1)
  }
  
  final protected def pushObject(values: Iterator[(String, J)], gen: JsonGenerator): Unit = {
    gen.writeStartObject()
    values match {
      case it: AbstractIterator[Any] =>
        instructions.add(it)
      case it: Iterator[Any] =>
        instructions.add(new WrappedIterator(it))
    }
    path.add("")
  }
  
  final protected def reportError(message: String): Unit = {
    if (path.size <= 1) {
      _errors ::= s"Serialization error at top level value: ${message}"
    }
    else {
      val sb = new java.lang.StringBuilder("Serialization error at ")
      var i = 1
      while (i < path.size) {
        path.get(i) match {
          case index: Integer =>
            sb.append('[')
            sb.append(index)
            sb.append(']')
          case name: String =>
            sb.append('.')
            sb.append(name)
        }
        
        i += 1
      }
      
      _errors ::= s"Serialization error at ${sb}: ${message}"
    }
  }
  
  def errors = _errors
  
  def drainErrors() = {
    val t = _errors
    _errors = Nil
    t
  }
  
  pushOne(input)
}

object JsonEmitter
{
  private class WrappedIterator[T](underlying: Iterator[T]) extends AbstractIterator[T]
  {
    override def hasNext = underlying.hasNext
  
    override def next() = underlying.next()
  }
}
package zygf.jackshaft.impl

import java.io.OutputStream

import scala.collection.AbstractIterator

import com.fasterxml.jackson.core.io.NumberOutput

class JsonPrinter[J](private val middleware: PrintingMiddleware[J],
                     private val out: OutputStream,
                     bufferSize: Int = 128)
{
  import JsonPrinter._
  
  private val buffer = new Array[Byte](bufferSize)
  
  private var instructions = new Array[Byte](8)
  private var arrayIterators = new Array[AbstractIterator[J]](8)
  private var objectIterators = new Array[AbstractIterator[(String, J)]](8)
  private var path = new Array[Any](8)
  
  private var depth = 0
  private var strValue: String = null
  private var strOffset: Int = 0
  
  private var _errors = Nil: List[String]
  
  private def grow(): Unit = {
    val l = instructions.length << 1
    instructions = java.util.Arrays.copyOf(instructions, l)
    arrayIterators = java.util.Arrays.copyOf(arrayIterators, l)
    objectIterators = java.util.Arrays.copyOf(objectIterators, l)
    path = java.util.Arrays.copyOf(path.asInstanceOf[Array[AnyRef]], l).asInstanceOf[Array[Any]]
  }
  
  final def start(value: J): Unit = {
    require(depth == 0)
    middleware.emit(value, this)
  }
  
  final def continue(): Boolean = {
    val at = depth - 1
    if (at < 0)
      return true
    
    instructions(at) match {
      case 0 =>
        val s = strValue
        var until = strOffset + 1024
        if (until >= s.length) {
          until = s.length
          emitStringContents(s, strOffset, until)
          strValue = null
          out.write('"')
          depth -= 1
        }
        else
          emitStringContents(s, strOffset, until)
        
      case insn @ (1 | 2) =>
        val it = objectIterators(at)
        if (it.hasNext) {
          val pair = it.next()
          
          path(at) = pair._1
          
          if (pair._1 ne null) {
            if (insn == 2)
              emitObjectSep()
            else
              instructions(at) = 2
            
            emitWholeString(pair._1)
            emitFieldSep()
            middleware.emit(pair._2, this)
          }
          else {
            reportError("null object key")
          }
  
          if (depth == at + 1 /* nothing new pushed */ && !it.hasNext) {
            emitEndObject()
            objectIterators(at) = null
            depth -= 1
          }
        }
        else {
          emitEndObject()
          objectIterators(at) = null
          depth -= 1
        }
        
      case insn @ (3 | 4) =>
        val it = arrayIterators(at)
        if (it.hasNext) {
          val elem = it.next()
          
          path(at) = path(at).asInstanceOf[Integer] + 1
          
          if (insn == 4)
            emitArraySep()
          else
            instructions(at) = 4
          
          middleware.emit(elem, this)
          
          if (depth == at + 1 /* nothing new pushed */ && !it.hasNext) {
            emitEndArray()
            arrayIterators(at) = null
            depth -= 1
          }
        }
        else {
          emitEndArray()
          arrayIterators(at) = null
          depth -= 1
        }
    }
    
    false
  }
  
  private def emitStartObject(): Unit = out.write('{')
  
  private def emitFieldSep(): Unit = out.write(':')
  
  private def emitObjectSep(): Unit = out.write(',')
  
  private def emitEndObject(): Unit = out.write('}')
  
  private def emitStartArray(): Unit = out.write('[')
  
  private def emitArraySep(): Unit = out.write(',')
  
  private def emitEndArray(): Unit = out.write(']')
  
  private def emitWholeString(s: String): Unit = {
    out.write('"')
    emitStringContents(s, 0, s.length)
    out.write('"')
  }
  
  private def emitStringContents(s: String, offset: Int, until: Int): Unit = {
    val buffer = this.buffer
    val L = buffer.length - 6
    var i = offset
    var b = 0
    while (i < until) {
      val c = s.charAt(i)
      
      if (c >= 32 && c <= 126) {
        buffer(b) = c.toByte
        b += 1
      }
      else {
        c match {
          case '"' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = '"'
            b += 1
          case '\\' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = '\\'
            b += 1
          case '\b' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = 'b'
            b += 1
          case '\f' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = 'f'
            b += 1
          case '\n' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = 'n'
            b += 1
          case '\r' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = 'r'
            b += 1
          case '\t' =>
            buffer(b) = '\\'
            b += 1
            buffer(b) = 't'
            b += 1
          case _ =>
            val n = c: Int
            val hex = hexDigits
            buffer(b) = '\\'
            b += 1
            buffer(b) = 'u'
            b += 1
            buffer(b) = hex(n >> 12)
            b += 1
            buffer(b) = hex((n >> 8) & 0xF)
            b += 1
            buffer(b) = hex((n >> 4) & 0xF)
            b += 1
            buffer(b) = hex(n & 0xF)
            b += 1
        }
      }
      
      if (b >= L) {
        out.write(buffer, 0, b)
        b = 0
      }
      
      i += 1
    }
    
    if (b > 0) {
      out.write(buffer, 0, b)
      b = 0
    }
  }
  
  // public API
  
  final def printRaw(value: String): Unit = {
    emitStringContents(value, 0, value.length)
  }
  
  final def printNull(): Unit = out.write(NULL)
  
  final def printTrue(): Unit = out.write(TRUE)
  
  final def printFalse(): Unit = out.write(FALSE)
  
  final def printBoolean(value: Boolean): Unit = {
    if (value)
      printTrue()
    else
      printFalse()
  }
  
  final def printInt(value: Int): Unit = {
    val n = NumberOutput.outputInt(value, buffer, 0)
    out.write(buffer, 0, n)
  }
  
  final def printLong(value: Long): Unit = {
    val n = NumberOutput.outputLong(value, buffer, 0)
    out.write(buffer, 0, n)
  }
  
  final def printString(value: String): Unit = {
    if (value.length <= 1024) {
      emitWholeString(value)
    }
    else {
      out.write('"')
      
      val depth = this.depth
  
      if (depth >= instructions.length)
        grow()
      
      instructions(depth) = 0
      strValue = value
      strOffset = 0
  
      this.depth = depth + 1
    }
  }
  
  final def printEmptyArray(): Unit = {
    emitStartArray()
    emitEndArray()
  }
  
  
  final def printArray(values: Iterable[J]): Unit = {
    if (values.isEmpty)
      printEmptyArray()
    else
      printArray(values.iterator)
  }
  
  final def printArray(iterator: Iterator[J]): Unit = {
    emitStartArray()
    if (! iterator.hasNext)
      return emitEndArray()
    
    val depth = this.depth
    
    if (depth >= instructions.length)
      grow()
    
    instructions(depth) = 3
    path(depth) = -1
    
    iterator match {
      case it: AbstractIterator[J] @unchecked =>
        arrayIterators(depth) = it
      case it =>
        arrayIterators(depth) = new WrappedIterator(it)
    }
    
    this.depth = depth + 1
  }
  
  final def printEmptyObject(): Unit = {
    emitStartObject()
    emitEndObject()
  }
  
  final def printObject(values: Iterable[(String, J)]): Unit = {
    if (values.isEmpty)
      printEmptyObject()
    else
      printObject(values.iterator)
  }
  
  final def printObject(iterator: Iterator[(String, J)]): Unit = {
    emitStartObject()
    if (! iterator.hasNext)
      return emitEndObject()
    
    val depth = this.depth
    
    if (depth >= instructions.length)
      grow()
    
    instructions(depth) = 1
    path(depth) = -1
    
    iterator match {
      case it: AbstractIterator[(String, J)] @unchecked =>
        objectIterators(depth) = it
      case it =>
        objectIterators(depth) = new WrappedIterator(it)
    }
    
    if (depth >= instructions.length)
      grow()
  
    this.depth = depth + 1
  }
  
  final def reportError(message: String): Unit = {
    if (depth < 1) {
      _errors ::= s"Serialization error at top level value: ${message}"
    }
    else {
      val sb = new java.lang.StringBuilder("Serialization error at ")
      var i = 0
      while (i < depth) {
        path(i) match {
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
}

object JsonPrinter
{
  private val NULL  = Array[Byte]('n', 'u', 'l', 'l')
  private val TRUE  = Array[Byte]('t', 'r', 'u', 'e')
  private val FALSE = Array[Byte]('f', 'a', 'l', 's', 'e')
  
  private class WrappedIterator[T](underlying: Iterator[T]) extends AbstractIterator[T]
  {
    override def hasNext = underlying.hasNext
  
    override def next() = underlying.next()
  }
  
  private val hexDigits = Array[Byte]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
}
package zygf.jackshaft.impl

import scala.annotation.switch
import scala.collection.AbstractIterator

import com.fasterxml.jackson.core.io.NumberOutput
import zygf.jackshaft.conf.StreamingMode

final class JsonPrinter[J](private val middleware: PrintingMiddleware[J])
{
  import JsonPrinter._
  
  private val buffer = new Array[Byte](4096) // TODO: refactor member into argument, so we can use a thread local buffer
  private val watermark = buffer.length - 32
  private var written = 0
  
  private var instructions = new Array[Byte](8)
  private var arrayIterators = new Array[AbstractIterator[J]](8)
  private var objectIterators = new Array[AbstractIterator[(String, J)]](8)
  private var path = new Array[Any](8)
  
  private var depth = 0
  private var strValue: String = null
  private var strOffset: Int = 0
  private var memberValue: (String, J) = null
  
  private var _errors = Nil: List[String]
  
  def isEmpty = written == 0
  
  def nonEmpty = written > 0
  
  private def grow(): Unit = {
    val l = instructions.length << 1
    instructions = java.util.Arrays.copyOf(instructions, l)
    arrayIterators = java.util.Arrays.copyOf(arrayIterators, l)
    objectIterators = java.util.Arrays.copyOf(objectIterators, l)
    path = java.util.Arrays.copyOf(path.asInstanceOf[Array[AnyRef]], l).asInstanceOf[Array[Any]]
  }
  
  def drain(into: Array[Byte], offset: Int): Int = {
    val w = written
    System.arraycopy(buffer, 0, into, offset, w)
    written = 0
    w
  }
  
  def drain(into: java.lang.StringBuilder): Int = {
    val w = written
    var i = 0
    while (i < w) {
      into.append(buffer(i).toChar)
      i += 1
    }
    written = 0
    w
  }
  
  def drain(into: scala.StringBuilder): Int = {
    val w = written
    var i = 0
    while (i < w) {
      into.append(buffer(i).toChar)
      i += 1
    }
    written = 0
    w
  }
  
  def drainAs[B >: Null](force: Boolean = false)(implicit B: WrapsByteArray[B]): B = {
    if (written > 0 && (force || written >= watermark)) {
      val w = written
      written = 0
      B.copy(buffer, 0, w)
    }
    else
      null
  }
  
  def start(value: J): Unit = {
    require(depth == 0)
    middleware.emit(value, this)
  }
  
  def continue(): Boolean = {
    val oldDepth = depth
    val at = oldDepth - 1
    if (at < 0)
      return true
    
    if (written >= watermark)
      return false
    
    (instructions(at): @switch) match {
      case 0 =>
        val s = strValue
        val L = s.length
        val n = emitStringContents(s, strOffset, L)
        if (n == L) {
          strValue = null
          buffer(written) = '"'
          written += 1
          depth -= 1
          return false
        }
        else {
          strOffset = n
        }
      
      case insn @ (1 | 3) =>
        // 1 - first object member
        // 2 - first object member, key done
        // 3 - next object member
        // 4 - next object member, key done
        
        val it = objectIterators(at)
        if (it.hasNext) {
          val pair = it.next()
          
          path(at) = pair._1
          
          if (pair._1 ne null) {
            if (insn == 3)
              emitObjectSep()
            
            printString(pair._1)
            
            if (depth > oldDepth) {
              instructions(at) = (insn + 1).toByte
              memberValue = pair
            }
            else {
              emitFieldSep()
              middleware.emit(pair._2, this)
              instructions(at) = 3
            }
          }
          else {
            reportError("null object key")
          }
          
          if (depth == oldDepth /* nothing new pushed */ && !it.hasNext) {
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
      
      case insn @ (2 | 4) =>
        // object field continuation
        emitFieldSep()
        middleware.emit(memberValue._2, this)
        instructions(at) = 3
      
      case insn @ (-1 | -2) =>
        val it = arrayIterators(at)
        if (it.hasNext) {
          val elem = it.next()
          
          path(at) = path(at).asInstanceOf[Integer] + 1
          
          if (insn == -2)
            emitArraySep()
          else
            instructions(at) = -2
          
          middleware.emit(elem, this)
          
          if (depth == oldDepth /* nothing new pushed */ && !it.hasNext) {
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
  
  private def emitStartObject(): Unit = {
    buffer(written) = '{'
    written += 1
  }
  
  private def emitFieldSep(): Unit = {
    buffer(written) = ':'
    written += 1
  }
  
  private def emitObjectSep(): Unit = {
    buffer(written) = ','
    written += 1
  }
  
  private def emitEndObject(): Unit = {
    buffer(written) = '}'
    written += 1
  }
  
  private[impl] def emitStartArray(): Unit = {
    buffer(written) = '['
    written += 1
  }
  
  private def emitArraySep(): Unit = {
    buffer(written) = ','
    written += 1
  }
  
  private def emitEndArray(): Unit = {
    buffer(written) = ']'
    written += 1
  }
  
  private def emitQuote(): Unit = {
    buffer(written) = '"'
    written += 1
  }
  
  @inline private def emitStringSwitch(c: Char, buffer: Array[Byte], b0: Int): Int = {
    var b = b0
    
    (c: @switch) match {
      case c @ (' ' | '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | ']' | '^' | '_' | '`' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | '{' | '|' | '}' | '~') =>
        buffer(b) = c.toByte
        b += 1
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
      case c =>
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
    
    b
  }
  
  @noinline private def emitStringLoopSlow(s: String, buffer: Array[Byte], offset: Int, until: Int): Int = {
    var i = offset
    var b = written
    val watermark = this.watermark
    while (i < until && b < watermark) {
      b = emitStringSwitch(s.charAt(i), buffer, b)
      i += 1
    }
    written = b
    i
  }
  
  @noinline private def emitStringLoopFast(s: String, buffer: Array[Byte], offset: Int, until: Int): Int = {
    var i = offset
    var b = written
    while (i < until) {
      b = emitStringSwitch(s.charAt(i), buffer, b)
      i += 1
    }
    written = b
    i
  }
  
  private def emitStringContents(s: String, offset: Int, until: Int): Int = {
    if (written + (until - offset) * 6 > this.watermark)
      emitStringLoopSlow(s, buffer, offset, until)
    else
      emitStringLoopFast(s, buffer, offset, until)
  }
  
  // public API
  
  def printRaw(value: String): Unit = { // TODO: incremental
    emitStringContents(value, 0, value.length)
  }
  
  def printNull(): Unit = {
    val buffer = this.buffer
    var b = written
    buffer(b) = 'n'
    b += 1
    buffer(b) = 'u'
    b += 1
    buffer(b) = 'l'
    b += 1
    buffer(b) = 'l'
    written = b + 1
  }
  
  def printTrue(): Unit = {
    val buffer = this.buffer
    var b = written
    buffer(b) = 't'
    b += 1
    buffer(b) = 'r'
    b += 1
    buffer(b) = 'u'
    b += 1
    buffer(b) = 'e'
    written = b + 1
  }
  
  def printFalse(): Unit = {
    val buffer = this.buffer
    var b = written
    buffer(b) = 'f'
    b += 1
    buffer(b) = 'a'
    b += 1
    buffer(b) = 'l'
    b += 1
    buffer(b) = 's'
    b += 1
    buffer(b) = 'e'
    written = b + 1
  }
  
  def printBoolean(value: Boolean): Unit = {
    if (value)
      printTrue()
    else
      printFalse()
  }
  
  def printInt(value: Int): Unit = {
    written += NumberOutput.outputInt(value, buffer, written)
  }
  
  def printLong(value: Long): Unit = {
    written += NumberOutput.outputLong(value, buffer, written)
  }
  
  def printString(value: String): Unit = {
    emitQuote()
    
    val L = value.length
    val n = emitStringContents(value, 0, L)
    if (n == L)
      emitQuote()
    else {
      val depth = this.depth
      
      if (depth >= instructions.length)
        grow()
      
      instructions(depth) = 0
      strOffset = n
      strValue = value
      
      this.depth = depth + 1
    }
  }
  
  def printEmptyArray(): Unit = {
    emitStartArray()
    emitEndArray()
  }
  
  
  def printArray(values: Iterable[J]): Unit = {
    if (values.isEmpty)
      printEmptyArray()
    else
      printArray(values.iterator)
  }
  
  def printArray(iterator: Iterator[J]): Unit = {
    emitStartArray()
    if (! iterator.hasNext)
      return emitEndArray()
    
    val depth = this.depth
    
    if (depth >= instructions.length)
      grow()
    
    instructions(depth) = -1
    path(depth) = -1
    
    iterator match {
      case it: AbstractIterator[J] =>
        arrayIterators(depth) = it
      case it =>
        arrayIterators(depth) = new WrappedIterator(it)
    }
    
    this.depth = depth + 1
  }
  
  def printEmptyObject(): Unit = {
    emitStartObject()
    emitEndObject()
  }
  
  def printObject(values: Iterable[(String, J)]): Unit = {
    if (values.isEmpty)
      printEmptyObject()
    else
      printObject(values.iterator)
  }
  
  def printObject(iterator: Iterator[(String, J)]): Unit = {
    emitStartObject()
    if (! iterator.hasNext)
      return emitEndObject()
    
    val depth = this.depth
    
    if (depth >= instructions.length)
      grow()
    
    instructions(depth) = 1
    path(depth) = -1
    
    iterator match {
      case it: AbstractIterator[(String, J)] =>
        objectIterators(depth) = it
      case it =>
        objectIterators(depth) = new WrappedIterator(it)
    }
    
    if (depth >= instructions.length)
      grow()
    
    this.depth = depth + 1
  }
  
  private[impl] def emitStreamingSep(mode: StreamingMode): Unit = {
    buffer(written) = mode.separator.toByte
    written += 1
  }
  
  def reportError(message: String): Unit = {
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
  
  // TODO: handle errors
  
  def errors = _errors
  
  def drainErrors() = {
    val t = _errors
    _errors = Nil
    t
  }
}

object JsonPrinter
{
  // we're using AbstractIterator to avoid megamorphic `invokeinterface` calls,
  // so we need to wrap exotic iterators
  private class WrappedIterator[T](underlying: Iterator[T]) extends AbstractIterator[T]
  {
    override def hasNext = underlying.hasNext
    override def next() = underlying.next()
  }
  
  private val hexDigits = Array[Byte]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
}
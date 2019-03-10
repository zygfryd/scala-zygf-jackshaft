package zygf.jackshaft.impl

import scala.annotation.switch
import scala.collection.AbstractIterator

import com.fasterxml.jackson.core.io.NumberOutput
import zygf.jackshaft.conf.StreamingMode

final class JsonPrinter[J](private val middleware: PrintingMiddleware[J])
{
  import JsonPrinter._
  
  private var instructions = new Array[Byte](8)
  private var arrayIterators = new Array[AbstractIterator[J]](8)
  private var objectIterators = new Array[AbstractIterator[(String, J)]](8)
  private var path = new Array[Any](8)
  
  private var depth = 0
  private var strValue: String = null
  private var strOffset: Int = 0
  private var memberValue: (String, J) = null
  
  private var _errors = Nil: List[String]
  
  private def grow(): Unit = {
    val l = instructions.length << 1
    instructions = java.util.Arrays.copyOf(instructions, l)
    arrayIterators = java.util.Arrays.copyOf(arrayIterators, l)
    objectIterators = java.util.Arrays.copyOf(objectIterators, l)
    path = java.util.Arrays.copyOf(path.asInstanceOf[Array[AnyRef]], l).asInstanceOf[Array[Any]]
  }
  
  def start(buffer: Array[Byte], offset: Int, value: J): Int = {
    require(depth == 0)
    middleware.emit(value, this, buffer, offset)
  }
  
  def continue(buffer: Array[Byte], offset: Int): Int = {
    val oldDepth = depth
    val at = oldDepth - 1
    if (at < 0)
      return -1
    
    val watermark = buffer.length - 32
    
    if (offset >= watermark)
      return offset
    
    var written = offset
    
    (instructions(at): @switch) match {
      case 0 =>
        val s = strValue
        val L = s.length
        val p = emitStringContents(buffer, offset, s, strOffset, L)
        val n = getIndex(p)
        val nOffset = getOffset(p)
        if (n == L) {
          strValue = null
          depth -= 1
          buffer(nOffset) = '"'
          nOffset + 1
        }
        else {
          strOffset = n
          nOffset
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
              written = emitObjectSep(buffer, written)
            
            written = printString(buffer, written, pair._1)
            
            if (depth > oldDepth) {
              instructions(at) = (insn + 1).toByte
              memberValue = pair
            }
            else {
              written = emitFieldSep(buffer, written)
              written = middleware.emit(pair._2, this, buffer, written)
              instructions(at) = 3
            }
          }
          else {
            reportError("null object key")
          }
          
          if (depth == oldDepth /* nothing new pushed */ && !it.hasNext) {
            written = emitEndObject(buffer, written)
            objectIterators(at) = null
            depth -= 1
          }
        }
        else {
          written = emitEndObject(buffer, written)
          objectIterators(at) = null
          depth -= 1
        }
        written
      
      case insn @ (2 | 4) =>
        // object field continuation
        written = emitFieldSep(buffer, written)
        written = middleware.emit(memberValue._2, this, buffer, written)
        instructions(at) = 3
        written
      
      case insn @ (-1 | -2) =>
        val it = arrayIterators(at)
        if (it.hasNext) {
          val elem = it.next()
          
          path(at) = path(at).asInstanceOf[Integer] + 1
          
          if (insn == -2)
            written = emitArraySep(buffer, written)
          else
            instructions(at) = -2
          
          written = middleware.emit(elem, this, buffer, written)
          
          if (depth == oldDepth /* nothing new pushed */ && !it.hasNext) {
            written = emitEndArray(buffer, written)
            arrayIterators(at) = null
            depth -= 1
          }
        }
        else {
          written = emitEndArray(buffer, written)
          arrayIterators(at) = null
          depth -= 1
        }
        written
    }
  }
  
  @inline private def emitStartObject(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = '{'
    offset + 1
  }
  
  @inline private def emitFieldSep(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = ':'
    offset + 1
  }
  
  @inline private def emitObjectSep(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = ','
    offset + 1
  }
  
  @inline private def emitEndObject(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = '}'
    offset + 1
  }
  
  @inline private[impl] def emitStartArray(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = '['
    offset + 1
  }
  
  @inline private def emitArraySep(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = ','
    offset + 1
  }
  
  @inline private def emitEndArray(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = ']'
    offset + 1
  }
  
  @inline private def emitQuote(buffer: Array[Byte], offset: Int): Int = {
    buffer(offset) = '"'
    offset + 1
  }
  
  @inline private def emitStringSwitch(buffer: Array[Byte], c: Char, offset: Int): Int = {
    var b = offset
    
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
  
  @inline private def toPair(offset: Int, index: Int): Long = (offset.toLong << 32) | (index & 0xffffffffL)
  @inline private def getOffset(pair: Long): Int = (pair >> 32).toInt
  @inline private def getIndex(pair: Long): Int = pair.toInt
  
  @noinline private def emitStringLoopSlow(buffer: Array[Byte], offset: Int, s: String, sOffset: Int, until: Int): Long = {
    var i = sOffset
    var b = offset
    val watermark = buffer.length - 32
    while (i < until && b < watermark) {
      b = emitStringSwitch(buffer, s.charAt(i), b)
      i += 1
    }
    toPair(b, i)
  }
  
  @noinline private def emitStringLoopFast(buffer: Array[Byte], offset: Int, s: String, sOffset: Int, until: Int): Long = {
    var i = sOffset
    var b = offset
    while (i < until) {
      b = emitStringSwitch(buffer, s.charAt(i), b)
      i += 1
    }
    toPair(b, i)
  }
  
  private def emitStringContents(buffer: Array[Byte], offset: Int, s: String, sOffset: Int, until: Int): Long = {
    if (offset + (until - sOffset) * 6 + 32 > buffer.length)
      emitStringLoopSlow(buffer, offset, s, sOffset, until)
    else
      emitStringLoopFast(buffer, offset, s, sOffset, until)
  }
  
  // public API
  
  def printRaw(buffer: Array[Byte], offset: Int, value: String): Int = { // TODO: incremental
    getOffset(emitStringContents(buffer, offset, value, 0, value.length))
  }
  
  def printNull(buffer: Array[Byte], offset: Int): Int = {
    var b = offset
    buffer(b) = 'n'
    b += 1
    buffer(b) = 'u'
    b += 1
    buffer(b) = 'l'
    b += 1
    buffer(b) = 'l'
    b + 1
  }
  
  def printTrue(buffer: Array[Byte], offset: Int): Int = {
    var b = offset
    buffer(b) = 't'
    b += 1
    buffer(b) = 'r'
    b += 1
    buffer(b) = 'u'
    b += 1
    buffer(b) = 'e'
    b + 1
  }
  
  def printFalse(buffer: Array[Byte], offset: Int): Int = {
    var b = offset
    buffer(b) = 'f'
    b += 1
    buffer(b) = 'a'
    b += 1
    buffer(b) = 'l'
    b += 1
    buffer(b) = 's'
    b += 1
    buffer(b) = 'e'
    b + 1
  }
  
  @inline def printBoolean(buffer: Array[Byte], offset: Int, value: Boolean): Int = {
    if (value)
      printTrue(buffer, offset)
    else
      printFalse(buffer, offset)
  }
  
  @inline def printInt(buffer: Array[Byte], offset: Int, value: Int): Int = {
    NumberOutput.outputInt(value, buffer, offset)
  }
  
  @inline def printLong(buffer: Array[Byte], offset: Int, value: Long): Int = {
    NumberOutput.outputLong(value, buffer, offset)
  }
  
  def printString(buffer: Array[Byte], offset: Int, value: String): Int = {
    val written = emitQuote(buffer, offset)
    
    val L = value.length
    val p = emitStringContents(buffer, written, value, 0, L)
    val n = getIndex(p)
    if (n == L)
      emitQuote(buffer, getOffset(p))
    else {
      val depth = this.depth
      
      if (depth >= instructions.length)
        grow()
      
      instructions(depth) = 0
      strOffset = n
      strValue = value
      
      this.depth = depth + 1
      
      getOffset(p)
    }
  }
  
  @inline def printEmptyArray(buffer: Array[Byte], offset: Int): Int = {
    emitEndArray(buffer, emitStartArray(buffer, offset))
  }
  
  @inline def printArray(buffer: Array[Byte], offset: Int, values: Iterable[J]): Int = {
    if (values.isEmpty)
      printEmptyArray(buffer, offset)
    else
      printArray(buffer, offset, values.iterator)
  }
  
  def printArray(buffer: Array[Byte], offset: Int, iterator: Iterator[J]): Int = {
    val written = emitStartArray(buffer, offset)
    
    if (! iterator.hasNext)
      return emitEndArray(buffer, written)
    
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
    
    written
  }
  
  @inline def printEmptyObject(buffer: Array[Byte], offset: Int): Int = {
    emitEndObject(buffer, emitStartObject(buffer, offset))
  }
  
  @inline def printObject(buffer: Array[Byte], offset: Int, values: Iterable[(String, J)]): Int = {
    if (values.isEmpty)
      printEmptyObject(buffer, offset)
    else
      printObject(buffer, offset, values.iterator)
  }
  
  def printObject(buffer: Array[Byte], offset: Int, iterator: Iterator[(String, J)]): Int = {
    val written = emitStartObject(buffer, offset)
    
    if (! iterator.hasNext)
      return emitEndObject(buffer, written)
    
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
    
    written
  }
  
  @inline private[impl] def emitStreamingSep(buffer: Array[Byte], offset: Int, mode: StreamingMode): Int = {
    buffer(offset) = mode.separator.toByte
    offset + 1
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
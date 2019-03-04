package zygf.jackshaft.impl

import java.nio.ByteBuffer
import java.util.function.{Consumer, Supplier}

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.async.ByteArrayFeeder

class ByteBufferParser[J >: Null](val middleware: ParsingMiddleware[J])
{
  import ByteBufferParser._
  
  private val jax = factory.createNonBlockingByteArrayParser()
  private val feeder = jax.getNonBlockingInputFeeder().asInstanceOf[ByteArrayFeeder]
  
  /** Parse a single value synchronously */
  def parseValue(input: Iterator[ByteBuffer]): J = {
    while (input.hasNext) {
      val bb = input.next()
      if (bb.hasArray) {
        val off = bb.arrayOffset
        feeder.feedInput(bb.array, off + (bb.position: Int), off + (bb.limit: Int))
        val result = middleware.parseValue(jax)
        if (null != result)
          return result
      }
      else {
        // we can't read this ByteBuffer without copying...
        val buf = threadBuffer.get
        val copy = bb//.asReadOnlyBuffer() // let's assume for now, that if akka-http gives us the buffer, we may consume it
        var left = bb.remaining
        while (left > 0) {
          val chunk = left min 4096
          copy.get(buf, 0, chunk)
          left -= chunk
          
          feeder.feedInput(buf, 0, chunk)
          val result = middleware.parseValue(jax)
          if (null != result)
            return result
        }
      }
    }
    
    feeder.endOfInput()
    middleware.parseValue(jax)
  }
  
  /** Parse values from a stream of byte buffers asynchronously */
  def parseAsync(input: Iterator[ByteBuffer], mode: ParsingMode)(callback: Consumer[J]): Boolean = {
    while (input.hasNext) {
      val bb = input.next()
      if (bb.hasArray) {
        val off = bb.arrayOffset
        feeder.feedInput(bb.array, off + (bb.position: Int), off + (bb.limit: Int))
        if (middleware.parseAsync(jax, mode, callback)) {
          return true
        }
      }
      else {
        // we can't read this ByteBuffer without copying...
        val buf = threadBuffer.get
        val copy = bb//.asReadOnlyBuffer() // let's assume for now, that if akka-http gives us the buffer, we may consume it
        var left = bb.remaining
        while (left > 0) {
          val chunk = left min 4096
          copy.get(buf, 0, chunk)
          left -= chunk
          
          feeder.feedInput(buf, 0, chunk)
          if (middleware.parseAsync(jax, mode, callback)) {
            return true
          }
        }
      }
    }
    
    false
  }
  
  def finishAsync(mode: ParsingMode)(callback: Consumer[J]): Boolean = {
    feeder.endOfInput()
    middleware.parseAsync(jax, mode, callback)
  }
}

object ByteBufferParser
{
  private val factory = (new JsonFactory)
    .enable(JsonFactory.Feature.CANONICALIZE_FIELD_NAMES)
    .disable(JsonFactory.Feature.INTERN_FIELD_NAMES)
  
  private val threadBuffer = ThreadLocal.withInitial[Array[Byte]] {
    new Supplier[Array[Byte]] {
      override def get() = new Array[Byte](4096)
    }
  }
}
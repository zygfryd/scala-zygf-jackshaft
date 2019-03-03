package zygf.jackshaft.impl

import java.util.function.{Consumer, Supplier}

import scala.language.higherKinds

import akka.util.ByteString
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.async.ByteArrayFeeder

class ByteStringParser[P[J1, A1, M1] <: JacksonMiddleware[J1, A1, M1], J >: Null](val middleware: P[J, _, _])
{
  import ByteStringParser._
  
  private val jax = factory.createNonBlockingByteArrayParser()
  private val feeder = jax.getNonBlockingInputFeeder().asInstanceOf[ByteArrayFeeder]
  
  /** Parse a single value from a single ByteString */
  def parseValue(input: ByteString): J = {
    val it = input.asByteBuffers.iterator
    while (it.hasNext) {
      val bb = it.next()
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
  
  def parse(input: ByteString, mode: ParsingMode)(callback: Consumer[J]): Boolean = {
    val it = input.asByteBuffers.iterator
    while (it.hasNext) {
      val bb = it.next()
      if (bb.hasArray) {
        val off = bb.arrayOffset
        feeder.feedInput(bb.array, off + (bb.position: Int), off + (bb.limit: Int))
        if (middleware.parse(jax, mode, callback)) {
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
          if (middleware.parse(jax, mode, callback)) {
            return true
          }
        }
      }
    }
      
    false
  }
  
  def finish(mode: ParsingMode)(callback: Consumer[J]): Boolean = {
    feeder.endOfInput()
    middleware.parse(jax, mode, callback)
  }
}

object ByteStringParser
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
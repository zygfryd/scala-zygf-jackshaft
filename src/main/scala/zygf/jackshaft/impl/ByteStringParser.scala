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
        feeder.feedInput(bb.array, bb.position, bb.limit)
        val result = middleware.parseValue(jax)
        if (null != result)
          return result
      }
      else {
        // we can't read this ByteBuffer without copying...
        val buf = threadBuffer.get
        var offset = 0
        var left = (bb.limit: Int) - (bb.position: Int)
        while (left > 0) {
          val chunk = left min 4096
          bb.get(buf, offset, chunk)
          left -= chunk
          offset += chunk
          
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
        feeder.feedInput(bb.array, bb.position, bb.limit)
        if (middleware.parse(jax, mode, callback)) {
          return true
        }
      }
      else {
        // we can't read this ByteBuffer without copying...
        val buf = threadBuffer.get
        var offset = 0
        var left = (bb.limit: Int) - (bb.position: Int)
        while (left > 0) {
          val chunk = left min 4096
          bb.get(buf, offset, chunk)
          left -= chunk
          offset += chunk
          
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
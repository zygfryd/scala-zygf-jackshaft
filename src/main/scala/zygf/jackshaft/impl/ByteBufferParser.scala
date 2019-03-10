package zygf.jackshaft.impl

import java.nio.ByteBuffer
import java.util.function.Consumer

import com.fasterxml.jackson.core.async.ByteArrayFeeder
import zygf.jackshaft.conf.JackshaftConfig

final class ByteBufferParser[J](val parsing: ParsingMiddleware[J])(implicit config: JackshaftConfig = JackshaftConfig.default)
{
  private val jax = config.jacksonFactory.createNonBlockingByteArrayParser()
  private val feeder = jax.getNonBlockingInputFeeder().asInstanceOf[ByteArrayFeeder]
  private val wrapper = parsing.createJacksonWrapper()
  
  /** Parse a single value synchronously */
  def parseValue(input: Iterator[ByteBuffer]): J = {
    // I don't think it's necessary to call needModeInput here, because the middleware class
    // always keeps going until there's no more input
    while (input.hasNext) {
      val bb = input.next()
      if (bb.hasArray) {
        val off = bb.arrayOffset
        feeder.feedInput(bb.array, off + (bb.position: Int), off + (bb.limit: Int))
        val result = wrapper.parseValue(jax)
        if (null != result)
          return result
      }
      else {
        // we can't read this ByteBuffer without copying...
        val bufferProvider = config.tempBufferProvider
        val buf = bufferProvider.acquire()
        try {
          var left = bb.remaining
          while (left > 0) {
            val chunk = left min 4096
            bb.get(buf, 0, chunk)
            left -= chunk
    
            feeder.feedInput(buf, 0, chunk)
            val result = wrapper.parseValue(jax)
            if (null != result)
              return result
          }
        }
        finally {
          bufferProvider.release(buf)
        }
      }
    }
    
    feeder.endOfInput()
    wrapper.parseValue(jax)
  }
  
  /** Parse values from a stream of byte buffers asynchronously */
  def parseAsync(input: Iterator[ByteBuffer], mode: ParsingMode)(callback: Consumer[J]): Boolean = {
    // I don't think it's necessary to call needModeInput here, because the middleware class
    // always keeps going until there's no more input
    while (input.hasNext) {
      val bb = input.next()
      if (bb.hasArray) {
        val off = bb.arrayOffset
        feeder.feedInput(bb.array, off + (bb.position: Int), off + (bb.limit: Int))
        if (wrapper.parseAsync(jax, mode, callback)) {
          return true
        }
      }
      else {
        // we can't read this ByteBuffer without copying...
        val bufferProvider = config.tempBufferProvider
        val buf = bufferProvider.acquire()
        try {
          var left = bb.remaining
          while (left > 0) {
            val chunk = left min 4096
            bb.get(buf, 0, chunk)
            left -= chunk
    
            feeder.feedInput(buf, 0, chunk)
            if (wrapper.parseAsync(jax, mode, callback)) {
              return true
            }
          }
        }
        finally {
          bufferProvider.release(buf)
        }
      }
    }
    
    false
  }
  
  def finishAsync(mode: ParsingMode)(callback: Consumer[J]): Unit = {
    feeder.endOfInput()
    wrapper.parseAsync(jax, mode, callback)
    ()
  }
}

package zygf.jackshaft.impl

import java.io.OutputStream
import java.nio.ByteBuffer

// UNUSED: There's no point in reusing ByteBuffers to improve performance with Akka
// as it simply consumes them and allocates new copies for the data

final class ByteBufferOutputStream(bufferSize: Int = 512) extends OutputStream
{
  import ByteBufferOutputStream._
  
  private var nextSize = bufferSize
  private val maxSize = bufferSize max 65536
  private val buffers = new java.util.ArrayDeque[Buffer](4)
  private val ready = new java.util.ArrayDeque[ByteBuffer](4)
  private var current = new Buffer(nextSize)
  
  def waiting = ready.size
  def poll(): ByteBuffer = ready.pollFirst()
  
  private def nextBuffer(): Buffer = {
    val first = buffers.peekFirst()
    if ((first ne null) && first.reusable) {
      buffers.removeFirst()
      first.reset()
      first
    }
    else {
      nextSize = (nextSize << 1) min maxSize
      new Buffer(nextSize)
    }
  }
  
  override def write(b: Int) = {
    var cur = current
    
    try {
      if (cur eq null)
        cur = nextBuffer()
    
      val target = cur.backend
    
      target.put(b.toByte)
    
      if (!target.hasRemaining) {
        buffers.addLast(cur)
        ready.addLast(cur.send())
        cur = null
      }
    }
    finally {
      current = cur
    }
  }
  
  override def write(b: Array[Byte], offset: Int, length: Int): Unit = {
    var off  = offset
    var left = length
    var cur  = current
    
    try {
      while (left > 0) {
        if (cur eq null)
          cur = nextBuffer()
        
        val target = cur.backend
        val count = left min target.remaining
        
        require(target.remaining > 0)
        
        target.put(b, off, count)
        off += count
        left -= count
        
        if (!target.hasRemaining) {
          buffers.addLast(cur)
          ready.addLast(cur.send())
          cur = null
        }
      }
    }
    finally {
      current = cur
    }
  }
  
  def maybeFlush(): Unit = {
    if ((current ne null) && current.backend.remaining < (current.backend.capacity / 8)) {
      buffers.addLast(current)
      ready.addLast(current.send())
      current = null
    }
  }
  
  override def flush(): Unit = {
    if ((current ne null) && (current.backend.position: Int) > 0) {
      buffers.addLast(current)
      ready.addLast(current.send())
      current = null
    }
  }
  
  override def close(): Unit = {
    flush()
      
    super.close()
  }
}

object ByteBufferOutputStream
{
  private class Buffer(size: Int)
  {
    val array:    Array[Byte] = new Array[Byte](size)
    val backend:  ByteBuffer  = ByteBuffer.wrap(array)
    var frontend: ByteBuffer  = null
  
    def reusable = !frontend.hasRemaining
    
    def reset() = {
      require((frontend eq null) || reusable)
      backend.clear()
      ()
    }
    
    def send() = {
      require((frontend eq null) || reusable)
      backend.flip()
      frontend = backend.asReadOnlyBuffer()
      frontend
    }
  }
}

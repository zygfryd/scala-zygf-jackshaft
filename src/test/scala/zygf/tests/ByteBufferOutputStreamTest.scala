package zygf.tests

import java.nio.ByteBuffer

import org.scalatest.{Assertion, FunSuite}
import zygf.jackshaft.impl.ByteBufferOutputStream

class ByteBufferOutputStreamTest extends FunSuite
{
  def testLength(buf: ByteBuffer, len: Int): Assertion = {
    assert(buf ne null)
    assert(buf.remaining == len)
  }
  
  test("growing") {
    val buf = new ByteBufferOutputStream(1)
    
    (1 to 10).foreach { i =>
      buf.write(i)
    }
    
    testLength(buf.poll(), 1)
    testLength(buf.poll(), 2)
    testLength(buf.poll(), 4)
    
    assert(buf.poll() eq null)
    
    buf.close()
  
    testLength(buf.poll(), 3)
  }
  
  test("re-use") {
    val buf = new ByteBufferOutputStream(4)
    
    (1 to 10).foreach { _ =>
      (1 to 4).foreach { i =>
        buf.write(i)
      }
  
      val chunk = buf.poll()
      testLength(chunk, 4)
      assert(chunk.capacity == 4) // still the same chunk, not a new, bigger one
      
      (1 to 4).foreach { _ => chunk.get() } // consume
    }
  }
}

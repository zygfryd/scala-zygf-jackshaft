package zygf.jackshaft.conf

import java.util.function.Supplier

abstract class TempBufferProvider
{
  def acquire(): Array[Byte]
  def release(buffer: Array[Byte]): Unit
}

object TempBufferProvider
{
  val defaultSize = 4096
  
  abstract class NonReleasing extends TempBufferProvider
  {
    final override def release(buffer: Array[Byte]): Unit = ()
  }
  
  class Fresh(val size: Int) extends NonReleasing
  {
    override def acquire() = new Array[Byte](size)
  }
  
  object Fresh extends Fresh(defaultSize)
  
  class ThreadLocal(val size: Int) extends NonReleasing
  {
    private val threadBuffer = java.lang.ThreadLocal.withInitial[Array[Byte]] {
      new Supplier[Array[Byte]] {
        override def get() = new Array[Byte](size)
      }
    }
  
    override def acquire() = threadBuffer.get()
  }
  
  object ThreadLocal extends ThreadLocal(defaultSize)
}

package zygf.jackshaft.impl

import java.io.{ByteArrayOutputStream, OutputStream}

import zygf.jackshaft.conf.JackshaftConfig
import zygf.jackshaft.exceptions.PrintingException

abstract class PrintingMiddleware[J]
{
  def emit(json: J, printer: JsonPrinter[J], buffer: Array[Byte], offset: Int): Int
  
  def printString(json: J)(implicit config: JackshaftConfig): String = {
    printString(json, 16)
  }
  
  def printString(json: J, bufferSize: Int)(implicit config: JackshaftConfig): String = {
    val out = new ByteArrayOutputStream(bufferSize)
    printStream(json, out)
    out.toString("ASCII")
  }
  
  def printStream(json: J, out: OutputStream)(implicit config: JackshaftConfig): Int = {
    val printer = new JsonPrinter(this)
    val buf = config.tempBufferProvider.acquire()
    var n = 0
    
    try {
      var offset = printer.start(buf, 0, json)
      var nOffset = 0
      while ({ nOffset = printer.continue(buf, offset); nOffset } >= 0) {
        if (nOffset == offset) {
          out.write(buf, 0, offset)
          n += offset
          offset = 0
        }
        else
          offset = nOffset
        
        if (printer.errors ne Nil) {
          throw PrintingException(printer.errors.head)
        }
      }
      
      if (offset > 0) {
        out.write(buf, 0, offset)
        n += offset
      }
      
      n
    }
    finally {
      config.tempBufferProvider.release(buf)
    }
  }
}

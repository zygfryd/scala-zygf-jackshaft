package zygf.jackshaft.impl

import zygf.jackshaft.conf.JackshaftConfig

abstract class PrintingMiddleware[J]
{
  def emit(json: J, printer: JsonPrinter[J], buffer: Array[Byte], offset: Int): Int
  
  def printString(json: J)(implicit config: JackshaftConfig = JackshaftConfig.default): String = {
    val printer = new JsonPrinter(this)
    val buf = config.tempBufferProvider.acquire()
    
    try {
      val sb = new java.lang.StringBuilder()
      var offset = printer.start(buf, 0, json)
      
      var i = 0
      while (i < offset) {
        sb.append(buf(i).toChar)
        i += 1
      }
      
      while ({ offset = printer.continue(buf, 0); offset } >= 0) {
        i = 0
        while (i < offset) {
          sb.append(buf(i).toChar)
          i += 1
        }
      }
      
      sb.toString()
    }
    finally {
      config.tempBufferProvider.release(buf)
    }
  }
}

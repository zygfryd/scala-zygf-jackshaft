package zygf.jackshaft.impl

import zygf.jackshaft.conf.JackshaftConfig

abstract class PrintingMiddleware[J]
{
  def emit(json: J, printer: JsonPrinter[J]): Unit
  
  def printString(json: J)(implicit config: JackshaftConfig = JackshaftConfig.default): String = {
    val printer = new JsonPrinter(this)
    val sb = new java.lang.StringBuilder()
    printer.start(json)
    while (!printer.continue()) {
      printer.drain(sb)
    }
    sb.toString()
  }
}

package zygf.jackshaft.impl

abstract class PrintingMiddleware[J]
{
  def emit(json: J, printer: JsonPrinter[J]): Unit
}

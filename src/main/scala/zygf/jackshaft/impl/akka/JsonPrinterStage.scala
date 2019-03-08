package zygf.jackshaft.impl.akka

import scala.language.higherKinds

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.impl.{ByteBufferOutputStream, JsonPrinter, PrintingMiddleware}

class JsonPrinterStage[J](value: J,
                          val printing: PrintingMiddleware[J])
  extends GraphStage[SourceShape[ByteString]]
{
  private val bytesOut = Outlet[ByteString]("bytesOut")
  
  val shape = SourceShape(bytesOut)
  
  override def createLogic(inheritedAttributes: Attributes) = new GraphStageLogic(shape) with OutHandler
  {
    setHandler(bytesOut, this)
    
    val buffer = new ByteBufferOutputStream()
    val printer = new JsonPrinter(printing, buffer)
    var bytes = 0
    
    override def preStart(): Unit = {
      printer.start(value)
    }
    
    override def onPull(): Unit = {
      var bb = buffer.poll()
      var done = false
      while ((bb eq null) && !done) {
        done = printer.continue()
        if (done)
          buffer.close()
        else
          buffer.maybeFlush()
        bb = buffer.poll()
      }
      
      if (bb ne null) {
        bytes += bb.remaining
        push(bytesOut, ByteString(bb))
      }
      
      if (done)
        completeStage()
    }
  }
}

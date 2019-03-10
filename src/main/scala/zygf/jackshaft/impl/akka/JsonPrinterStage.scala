package zygf.jackshaft.impl.akka

import scala.language.higherKinds

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.conf.JackshaftConfig
import zygf.jackshaft.impl.{JsonPrinter, PrintingMiddleware}

class JsonPrinterStage[J](value: J,
                          val printing: PrintingMiddleware[J])
                         (implicit config: JackshaftConfig = JackshaftConfig.default)
  extends GraphStage[SourceShape[ByteString]]
{
  private val bytesOut = Outlet[ByteString]("bytesOut")
  
  val shape = SourceShape(bytesOut)
  
  override def createLogic(inheritedAttributes: Attributes) = new GraphStageLogic(shape) with OutHandler
  {
    setHandler(bytesOut, this)
    
    val printer = new JsonPrinter(printing)
    var started = false
    
    override def onPull(): Unit = {
      val buf = config.tempBufferProvider.acquire()
      try {
        var bs = ByteString.empty
        
        var offset = if (started) 0
        else {
          started = true
          printer.start(buf, 0, value)
        }
        
        var done = false
        
        while (! done) {
          val nOffset = printer.continue(buf, offset)
          if (nOffset < 0)
            done = true
          else if (nOffset > offset)
            offset = nOffset
          else {
            push(bytesOut, ByteString.fromArray(buf, 0, nOffset))
            return
          }
        }
        
        if (offset > 0) {
          push(bytesOut, ByteString.fromArray(buf, 0, offset))
        }
        
        completeStage()
      }
      finally {
        config.tempBufferProvider.release(buf)
      }
    }
  }
}

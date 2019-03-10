package zygf.jackshaft.impl.akka

import scala.language.higherKinds

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.impl.{JsonPrinter, PrintingMiddleware}

class JsonPrinterStage[J](value: J,
                          val printing: PrintingMiddleware[J])
  extends GraphStage[SourceShape[ByteString]]
{
  import Implicits._
  
  private val bytesOut = Outlet[ByteString]("bytesOut")
  
  val shape = SourceShape(bytesOut)
  
  override def createLogic(inheritedAttributes: Attributes) = new GraphStageLogic(shape) with OutHandler
  {
    setHandler(bytesOut, this)
    
    val printer = new JsonPrinter(printing)
    
    override def preStart(): Unit = {
      printer.start(value)
    }
    
    override def onPull(): Unit = {
      var bs = null: ByteString
      var done = false
      while ((bs eq null) && !done) {
        done = printer.continue()
        bs = printer.drainAs[ByteString](done)
      }

      if (bs ne null) {
        push(bytesOut, bs)
      }
      
      if (done)
        completeStage()
    }
  }
}

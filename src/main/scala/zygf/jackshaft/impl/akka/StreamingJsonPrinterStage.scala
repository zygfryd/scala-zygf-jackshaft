package zygf.jackshaft.impl.akka

import scala.language.higherKinds

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.conf.{JackshaftConfig, StreamingMode}
import zygf.jackshaft.impl._

class StreamingJsonPrinterStage[J](val printing: PrintingMiddleware[J])(implicit config: JackshaftConfig = JackshaftConfig.default)
  extends GraphStage[FlowShape[J, ByteString]]
{
  import StreamingJsonPrinterStage._
  
  private val jsonIn   = Inlet[J]("jsonIn")
  private val bytesOut = Outlet[ByteString]("bytesOut")
  
  val shape: FlowShape[J, ByteString] = FlowShape(jsonIn, bytesOut)
  
  override def createLogic(inheritedAttributes: Attributes) = new GraphStageLogic(shape) with InHandler with OutHandler {
    setHandlers(jsonIn, bytesOut, this)
  
    val printer  = new JsonPrinter(printing)
    var done     = true
    var opened   = false
    var separate = false
    val mode     = config.streamingMode
    val array    = mode eq StreamingMode.Array
  
    override def preStart(): Unit = {
      pull(jsonIn)
    }
    
    override def onPush(): Unit = {
      if (isAvailable(bytesOut)) {
        val buf = config.tempBufferProvider.acquire()
        try
          print(buf)
        finally
          config.tempBufferProvider.release(buf)
      }
    }
    
    def print(buf: Array[Byte]): Unit = {
      var offset = 0
      
      if (done) {
        if (isAvailable(jsonIn)) {
          if (separate) {
            offset = printer.emitStreamingSep(buf, offset, mode)
            separate = false
          }
          else if (array && !opened) {
            offset = printer.emitStartArray(buf, offset)
            opened = true
          }
          
          offset = printer.start(buf, offset, grab(jsonIn))
          
          done = false
          
          if (!isClosed(jsonIn))
            pull(jsonIn)
        }
      }
      
      while (! done) {
        val nOffset = printer.continue(buf, offset)
        if (nOffset < 0) {
          done = true
          separate = true
        }
        else if (nOffset > 0)
          offset = nOffset
        else {
          push(bytesOut, ByteString.fromArray(buf, 0, nOffset))
          return
        }
      }
      
      if (offset > 0) {
        push(bytesOut, ByteString.fromArray(buf, 0, offset))
      }
    }
    
    override def onPull(): Unit = {
      val buf = config.tempBufferProvider.acquire()
      try
        print(buf)
      finally
        config.tempBufferProvider.release(buf)
      
      if (done && isClosed(jsonIn)) {
        if (array)
          emit(bytesOut, closeArray)
        completeStage()
      }
    }
  
    override def onUpstreamFinish(): Unit = ()
  }
}

object StreamingJsonPrinterStage
{
  private val closeArray = ByteString(']')
}

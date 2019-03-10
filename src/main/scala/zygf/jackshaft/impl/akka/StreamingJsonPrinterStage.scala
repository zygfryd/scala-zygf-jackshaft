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
  import akka.Implicits._
  
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
      if (done) {
        done = false
  
        if (separate) {
          printer.emitStreamingSep(mode)
          separate = false
        }
        else if (array && ! opened) {
          printer.emitStartArray()
          opened = true
        }
        
        printer.start(grab(jsonIn))
        
        separate = printer.nonEmpty
        
        if (isClosed(jsonIn))
          completeStage()
        else
          pull(jsonIn)
      }
      else if (isClosed(jsonIn))
        completeStage()
      
      if (isAvailable(bytesOut)) {
        onPull()
      }
    }
    
    override def onPull(): Unit = {
      if (! done) {
        val bs = print(false)
        if (bs ne null)
          push(bytesOut, bs)
      }
      else if (isAvailable(jsonIn)) {
        onPush()
      }
    }
    
    override def onUpstreamFinish(): Unit = {
      var bbs = Vector.empty[ByteString]
      while (!done) {
        val bs = print(true)
        if (bs ne null)
          bbs :+= bs
      }
      
      if (array)
        bbs :+= closeArray
      
      if (! bbs.isEmpty)
        emitMultiple(bytesOut, bbs)
      
      completeStage()
    }
  
    def print(end: Boolean): ByteString = {
      var bs = printer.drainAs[ByteString](done)
      while (bs eq null) {
        done = printer.continue()
        bs = printer.drainAs[ByteString](done)
      }
      
      separate = done
      bs
    }
  }
}

object StreamingJsonPrinterStage
{
  private val closeArray = ByteString(']')
}

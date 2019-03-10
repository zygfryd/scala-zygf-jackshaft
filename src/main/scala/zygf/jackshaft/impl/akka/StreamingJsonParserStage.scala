package zygf.jackshaft.impl.akka

import java.util.function.Consumer

import scala.language.higherKinds
import scala.util.control.NonFatal

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.conf.JackshaftConfig
import zygf.jackshaft.impl.{ByteBufferParser, ParsingMiddleware}

class StreamingJsonParserStage[J](val parsing: ParsingMiddleware[J])(implicit config: JackshaftConfig = JackshaftConfig.default)
  extends GraphStage[FlowShape[ByteString, J]]
{
  private val bytesIn = Inlet[ByteString]("bytesIn")
  private val jsonOut = Outlet[J]("jsonOut")
  private val pMode = config.streamingMode.parsingMode
  
  val shape: FlowShape[ByteString, J] = FlowShape(bytesIn, jsonOut)
  
  override def createLogic(inheritedAttributes: Attributes) = new GraphStageLogic(shape) with InHandler with OutHandler {
    setHandlers(bytesIn, jsonOut, this)
    
    val parser = new ByteBufferParser(parsing)
    val queue = new java.util.ArrayDeque[J]()
    val consumer: Consumer[J] = new Consumer[J] {
      override def accept(json: J): Unit = queue.add(json)
    }
    
    override def onPush(): Unit = {
      try {
        parser.parseAsync(grab(bytesIn).asByteBuffers.iterator, pMode)(consumer)
      }
      catch {
        case NonFatal(e) =>
          failStage(e)
      }
      
      if (isAvailable(jsonOut)) {
        onPull()
      }
    }
  
    override def onPull(): Unit = {
      val json = queue.pollFirst()
      if (null != json) {
        push(jsonOut, json)
      }
      else if (isClosed(bytesIn)) {
        completeStage()
      }
      else {
        pull(bytesIn)
      }
    }
  
    override def onUpstreamFinish(): Unit = {
      try {
        parser.finishAsync(pMode)(consumer)
      }
      catch {
        case NonFatal(e) =>
          failStage(e)
      }

      if (! queue.isEmpty)
        emitMultiple(jsonOut, queue.iterator())
      
      completeStage()
    }
  }
}

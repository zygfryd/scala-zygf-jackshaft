package zygf.jackshaft.impl.akka

import java.util.function.Consumer

import scala.concurrent.{Future, Promise}
import scala.language.higherKinds
import scala.util.control.NonFatal

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.conf.JackshaftConfig
import zygf.jackshaft.exceptions.UnexpectedEndOfInputException
import zygf.jackshaft.impl.{ByteBufferParser, ParsingMiddleware, ParsingMode}

class JsonParserStage[J](val parsing: ParsingMiddleware[J])(implicit config: JackshaftConfig)
  extends GraphStageWithMaterializedValue[SinkShape[ByteString], Future[J]]
{
  private val bytesIn = Inlet[ByteString]("bytesIn")
    
  val shape = SinkShape(bytesIn)
  
  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes) = {
    val promise = Promise[J]()
    
    val logic = new GraphStageLogic(shape) with InHandler
    {
      setHandler(bytesIn, this)
      
      private val parser = new ByteBufferParser(parsing)
      private var result: Option[J] = None
      
      private val consumer: Consumer[J] = new Consumer[J] {
        override def accept(json: J): Unit = {
          if (result.isEmpty)   
            result = Some(json)
        }
      }
      
      override def preStart(): Unit = {
        pull(bytesIn)
      }
      
      override def onPush(): Unit = {
        if (result.isEmpty) {
          try {
            parser.parseAsync(grab(bytesIn).asByteBuffers.iterator, ParsingMode.VALUE)(consumer)
          }
          catch {
            case NonFatal(e) =>
              promise.failure(e)
              failStage(e)
              return
          }
        }
        else {
          // don't grab, discard trailing garbage
        }
        
        pull(bytesIn)
      }
      
      override def onUpstreamFinish(): Unit = {
        try {
          parser.finishAsync(ParsingMode.VALUE)(consumer)
        }
        catch {
          case NonFatal(e) =>
            promise.failure(e)
            failStage(e)
            return
        }
        
        complete()
      }
      
      override def onUpstreamFailure(e: Throwable): Unit = {
        super.onUpstreamFailure(e)
        promise.tryFailure(e)
      }
      
      private def complete(): Unit = {
        result match {
          case None =>
            failStage(UnexpectedEndOfInputException)
            promise.tryFailure(UnexpectedEndOfInputException)
          case Some(json) =>
            completeStage()
            promise.trySuccess(json)
        }
      }
    }
    
    (logic, promise.future)
  }
}

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

class JsonParserStage[J](val parsing: ParsingMiddleware[J])(implicit config: JackshaftConfig = JackshaftConfig.default)
  extends GraphStageWithMaterializedValue[SinkShape[ByteString], Future[J]]
{
  private val bytesIn = Inlet[ByteString]("bytesIn")
    
  val shape = SinkShape(bytesIn)
  
  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes) = {
    val promise = Promise[J]()
    
    val logic = new GraphStageLogic(shape) with InHandler
    {
      setHandler(bytesIn, this)
      
      val parser = new ByteBufferParser(parsing)
      
      val consumer: Consumer[J] = new Consumer[J] {
        override def accept(json: J): Unit = promise.trySuccess(json)
      }
      
      override def preStart(): Unit = {
        pull(bytesIn)
      }
      
      override def onPush(): Unit = {
        if (! promise.isCompleted) {
          try {
            parser.parseAsync(grab(bytesIn).asByteBuffers.iterator, ParsingMode.VALUE)(consumer)
          }
          catch {
            case NonFatal(e) =>
              promise.failure(e)
              failStage(e)
          }
        }
        
        if (isClosed(bytesIn))
          completeStage()
        else
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
        }
          
        if (promise.isCompleted)
          completeStage()
        else {
          promise.failure(UnexpectedEndOfInputException)
          failStage(UnexpectedEndOfInputException)
        }
      }
      
      override def onUpstreamFailure(e: Throwable): Unit = {
        super.onUpstreamFailure(e)
        promise.tryFailure(e)
      }
    }
    
    (logic, promise.future)
  }
}

package zygf.jackshaft.impl

import java.util.function.Consumer

import scala.concurrent.{Future, Promise}
import scala.language.higherKinds
import scala.util.control.NonFatal

import akka.stream._
import akka.stream.stage._
import akka.util.ByteString
import zygf.jackshaft.exceptions.UnexpectedEndOfInputException

abstract class JsonParserStage[P[J1, A1, M1] <: JacksonMiddleware[J1, A1, M1], J >: Null](val makeParser: () => ByteStringParser[P, J])
  extends GraphStageWithMaterializedValue[SinkShape[ByteString], Future[J]]
{
  private val bytesIn = Inlet[ByteString]("bytesIn")
    
  val shape = SinkShape(bytesIn)
  
  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes) = {
    val promise = Promise[J]()
    
    val logic = new GraphStageLogic(shape) with InHandler
    {
      setHandler(bytesIn, this)
      
      val parser = makeParser()
      
      val consumer: Consumer[J] = new Consumer[J] {
        override def accept(json: J): Unit = promise.trySuccess(json)
      }
      
      override def preStart(): Unit = {
        pull(bytesIn)
      }
      
      override def onPush(): Unit = {
        if (! promise.isCompleted) {
          try {
            parser.parse(grab(bytesIn), ParsingMode.VALUE)(consumer)
          }
          catch {
            case NonFatal(e) =>
              promise.failure(e)
              failStage(e)
          }
        }
        
        pull(bytesIn)
      }
      
      override def onUpstreamFinish(): Unit = {
        try {
          parser.finish(ParsingMode.VALUE)(consumer)
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

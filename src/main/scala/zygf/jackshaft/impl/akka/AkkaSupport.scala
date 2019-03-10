package zygf.jackshaft.impl.akka

import scala.concurrent.Future

import akka.NotUsed
import akka.http.scaladsl.marshalling.{Marshaller, ToByteStringMarshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.unmarshalling.{FromByteStringUnmarshaller, FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.{Keep, Source}
import akka.util.ByteString
import zygf.jackshaft.conf.JackshaftConfig
import zygf.jackshaft.exceptions.{PrintingException, UnexpectedEndOfInputException}
import zygf.jackshaft.impl.{ByteBufferParser, JsonPrinter, ParsingMiddleware, PrintingMiddleware}

abstract class AkkaSupport[J](val parsing: ParsingMiddleware[J],
                              val printing: PrintingMiddleware[J])
{
  protected def parseStrict(input: ByteString)
                           (implicit config: JackshaftConfig): Future[J] = {
    val parser = new ByteBufferParser(parsing)
    val json = parser.parseValue(input.asByteBuffers.iterator)
    if (null == json) {
      FastFuture.failed(UnexpectedEndOfInputException)
    }
    else
      FastFuture.successful(json)
  }
  
  protected def printStrict(json: J)
                           (implicit config: JackshaftConfig): ByteString = {
    val printer = new JsonPrinter(printing)
    val buf = config.tempBufferProvider.acquire()
    
    try {
      var bs = ByteString.empty
      var offset = printer.start(buf, 0, json)
      var done = false
      
      while (! done) {
        val nOffset = printer.continue(buf, offset)
        if (nOffset < 0)
          done = true
        else if (nOffset > offset)
          offset = nOffset
        else {
          bs ++= ByteString.fromArray(buf, 0, nOffset)
          offset = 0
        }
      }
      
      if (printer.errors ne Nil) {
        throw PrintingException(printer.errors.head)
      }
      
      if (offset > 0) {
        bs ++= ByteString.fromArray(buf, 0, offset)
      }
      
      bs
    }
    finally {
      config.tempBufferProvider.release(buf)
    }
  }
  
  protected def fromByteStringUnmarshaller(implicit config: JackshaftConfig): FromByteStringUnmarshaller[J] = {
    Unmarshaller.withMaterializer[ByteString, J] {
      implicit ec =>
        implicit mat =>
          input =>
            parseStrict(input)
    }
  }
  
  protected def toByteStringMarshaller(implicit config: JackshaftConfig): ToByteStringMarshaller[J] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      printStrict(json)
    }
  
  protected def fromEntityUnmarshaller(implicit config: JackshaftConfig): FromEntityUnmarshaller[J] = {
    Unmarshaller.withMaterializer[HttpEntity, J] {
      implicit ec =>
        implicit mat => {
          case HttpEntity.Strict(_, input) =>
            parseStrict(input)
          
          case entity =>
            entity.dataBytes.runWith(new JsonParserStage(parsing))
        }
    }.forContentTypes(`application/json`)
  }
  
  protected def toEntityMarshaller(implicit config: JackshaftConfig): ToEntityMarshaller[J] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      HttpEntity(`application/json`,
                 Source.fromGraph(new JsonPrinterStage(json, printing)))
    }
  
  protected def sourceFromEntityUnmarshaller(implicit config: JackshaftConfig): FromEntityUnmarshaller[Source[J, NotUsed]] = {
    Unmarshaller.withMaterializer[HttpEntity, Source[J, NotUsed]] {
      implicit ec =>
        implicit mat =>
          entity =>
            val source = entity
              .dataBytes
              .viaMat(new StreamingJsonParserStage(parsing))(Keep.right)
          
            FastFuture.successful(source)
    }
  }
  
  protected def sourceToEntityMarshaller(implicit config: JackshaftConfig): ToEntityMarshaller[Source[J, NotUsed]] = {
    Marshaller.withFixedContentType(`application/json`) { jsons =>
      HttpEntity(`application/json`,
                 jsons.via(new StreamingJsonPrinterStage(printing)))
    }
  }
  
}

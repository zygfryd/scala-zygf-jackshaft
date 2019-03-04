/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 * Copyright (C) 2019-2019 Marcin "Zygfryd" Pertek
 */

package zygf.jackshaft.spray

import scala.concurrent.Future
import scala.language.implicitConversions

import akka.NotUsed
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromByteStringUnmarshaller, FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.{Keep, Source}
import akka.util.ByteString
import spray.json._
import zygf.jackshaft.exceptions.UnexpectedEndOfInputException
import zygf.jackshaft.impl.akka.{JsonParserStage, StreamingJsonParserStage}
import zygf.jackshaft.impl.{ByteBufferParser, ParsingMode, StreamingJsonParserStage}

/**
  * A trait providing automatic to and from JSON marshalling/unmarshalling using an in-scope *spray-json* protocol.
  */
trait AkkaSprayJsonSupport {
  private def parseStrict(input: ByteString): Future[JsValue] = {
    val parser = new ByteBufferParser(new SprayMiddleware)
    val json = parser.parseValue(input.asByteBuffers.iterator)
    if (json eq null) {
      FastFuture.failed(UnexpectedEndOfInputException)
    }
    else
      FastFuture.successful(json)
  }
  
  implicit val singleJsValueFromByteStringUnmarshaller: FromByteStringUnmarshaller[JsValue] = {
    Unmarshaller.withMaterializer[ByteString, JsValue] {
      implicit ec =>
        implicit mat =>
          input =>
            parseStrict(input)
    }
  }
  
  implicit val singleJsValueFromEntityUnmarshaller: FromEntityUnmarshaller[JsValue] = {
    Unmarshaller.withMaterializer[HttpEntity, JsValue] {
      implicit ec =>
        implicit mat => {
          case HttpEntity.Strict(_, input) =>
            parseStrict(input)
          
          case entity =>
            entity.dataBytes.runWith(ByteStringsToValueStage)
        }
    }.forContentTypes(`application/json`)
  }
  
  implicit def singleJsonReadableFromByteStringUnmarshaller[T](implicit reader: RootJsonReader[T]): FromByteStringUnmarshaller[T] =
    singleJsValueFromByteStringUnmarshaller.map(jsonReader[T].read)
  
  implicit def singleJsonReadableFromEntityUnmarshaller[T](implicit reader: RootJsonReader[T]): FromEntityUnmarshaller[T] =
    singleJsValueFromEntityUnmarshaller.map(jsonReader[T].read)
  
  implicit def singleJsonReadableFromEntityUnmarshallerConverter[T](reader: RootJsonReader[T]): FromEntityUnmarshaller[T] =
    singleJsonReadableFromEntityUnmarshaller(reader)
  
  object ByteStringsToValueStage extends JsonParserStage(() => new ByteBufferParser(new SprayMiddleware))
  object ByteStringsToArrayStage extends StreamingJsonParserStage(ParsingMode.ARRAY, () => new ByteBufferParser(new SprayMiddleware))
  object ByteStringsToStreamStage extends StreamingJsonParserStage(ParsingMode.STREAM, () => new ByteBufferParser(new SprayMiddleware))
  
  implicit val streamingJsArrayFromEntityUnmarshaller: FromEntityUnmarshaller[Source[JsValue, NotUsed]] = {
    Unmarshaller.withMaterializer[HttpEntity, Source[JsValue, NotUsed]] {
      implicit ec =>
        implicit mat =>
          entity =>
            val source = entity
              .dataBytes
              .viaMat(ByteStringsToArrayStage)(Keep.right)
            
            FastFuture.successful(source)
    }
  }
  
  // TODO: redo writing?
  
  //#sprayJsonMarshallerConverter
  implicit def sprayJsonMarshallerConverter[T](writer: RootJsonWriter[T])(implicit printer: JsonPrinter = CompactPrinter): ToEntityMarshaller[T] =
    sprayJsonMarshaller[T](writer, printer)
  //#sprayJsonMarshallerConverter
  implicit def sprayJsonMarshaller[T](implicit writer: RootJsonWriter[T], printer: JsonPrinter = CompactPrinter): ToEntityMarshaller[T] =
    sprayJsValueMarshaller compose writer.write
  implicit def sprayJsValueMarshaller(implicit printer: JsonPrinter = CompactPrinter): ToEntityMarshaller[JsValue] =
    Marshaller.StringMarshaller.wrap(MediaTypes.`application/json`)(printer)
}

object AkkaSprayJsonSupport extends AkkaSprayJsonSupport


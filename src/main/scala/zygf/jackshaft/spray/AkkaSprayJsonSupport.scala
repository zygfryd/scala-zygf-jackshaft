/*
 * Copyright (C) 2009-2019 Lightbend Inc. <https://www.lightbend.com>
 * Copyright (C) 2019-2019 Marcin "Zygfryd" Pertek
 */

package zygf.jackshaft.spray

import scala.language.implicitConversions

import akka.NotUsed
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.unmarshalling.{FromByteStringUnmarshaller, FromEntityUnmarshaller}
import akka.stream.scaladsl.Source
import spray.json._
import zygf.jackshaft.conf.{JackshaftConfig, StreamingMode}
import zygf.jackshaft.impl.akka.AkkaSupport

/**
  * A trait providing automatic to and from JSON marshalling/unmarshalling using an in-scope *spray-json* protocol.
  */
abstract class AkkaSprayJsonSupport extends AkkaSupport(SprayParser, SprayPrinter)
{
  // IDK what needs these
//  implicit def sprayJsonUnmarshallerConverter[T](reader: JsonReader[T]): FromEntityUnmarshaller[T] =
//    sprayJsonUnmarshaller(reader)
  
  implicit def sprayJsValueUnmarshaller(implicit config: JackshaftConfig = JackshaftConfig.default) = fromEntityUnmarshaller
  
  implicit def sprayJsonUnmarshaller[T](implicit reader: JsonReader[T], config: JackshaftConfig = JackshaftConfig.default): FromEntityUnmarshaller[T] =
    sprayJsValueUnmarshaller.map(reader.read)
  
  implicit def sprayJsValueByteStringUnmarshaller(implicit config: JackshaftConfig = JackshaftConfig.default) = fromByteStringUnmarshaller
  
  implicit def sprayJsonByteStringUnmarshaller[T](implicit reader: JsonReader[T], config: JackshaftConfig = JackshaftConfig.default): FromByteStringUnmarshaller[T] =
    sprayJsValueByteStringUnmarshaller.map(reader.read)
  
  implicit def sprayJsValueSourceReader(implicit config: JackshaftConfig = JackshaftConfig.default): FromEntityUnmarshaller[Source[JsValue, NotUsed]] =
    sourceFromEntityUnmarshaller
  
  implicit def sprayJsonSourceReader[T](implicit reader: JsonReader[T],
                                        config: JackshaftConfig = JackshaftConfig.default): FromEntityUnmarshaller[Source[T, NotUsed]] =
    sourceFromEntityUnmarshaller.map[Source[T, NotUsed]] { source =>
      source.map(reader.read)
    }
  
  implicit def sprayJsValueMarshaller(implicit config: JackshaftConfig = JackshaftConfig.default) = toEntityMarshaller 
  
  implicit def sprayJsonMarshaller[T](implicit writer: RootJsonWriter[T]): ToEntityMarshaller[T] =
    toEntityMarshaller compose writer.write
  
  implicit def sprayJsValueSourceWriter(implicit config: JackshaftConfig = JackshaftConfig.default): ToEntityMarshaller[Source[JsValue, NotUsed]] =
    sourceToEntityMarshaller
  
  implicit def sprayJsonSourceWriter[T](implicit writer: JsonWriter[T],
                                        config: JackshaftConfig = JackshaftConfig.default): ToEntityMarshaller[Source[T, NotUsed]] =
    sourceToEntityMarshaller.compose { source =>
      source.map(writer.write)
    }
}

object AkkaSprayJsonSupport extends AkkaSprayJsonSupport

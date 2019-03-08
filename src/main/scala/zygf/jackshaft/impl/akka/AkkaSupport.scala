package zygf.jackshaft.impl.akka

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.stream.scaladsl.Source
import zygf.jackshaft.impl.PrintingMiddleware

// TODO: move parsing here too

abstract class AkkaSupport[J](val printing: PrintingMiddleware[J])
{
  protected val marshallerImpl: ToEntityMarshaller[J] =
    Marshaller.withFixedContentType(`application/json`) { json =>
      HttpEntity(`application/json`,
                 Source.fromGraph(new JsonPrinterStage(json, printing)))
    }
}

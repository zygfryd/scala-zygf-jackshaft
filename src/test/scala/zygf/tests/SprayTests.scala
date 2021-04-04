package zygf.tests

import java.util.function.Consumer

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{HttpEntity, MessageEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.async.ByteArrayFeeder
import spray.json._
import zygf.jackshaft.conf.{JackshaftConfig, StreamingMode}
import zygf.jackshaft.exceptions.PrintingException
import zygf.jackshaft.spray.{SprayParser, SprayPrinter}

class SprayTests extends org.scalatest.FunSuite
{
  implicit val sys = ActorSystem("SprayJsonSupportSpec")
  import sys.dispatcher
  
  // 2.11
  def consumer[T](from: T => Unit): Consumer[T] = new Consumer[T] {
    override def accept(t: T): Unit = from(t)
  }
  
  test("parseArray") {
    val input = "[1, true, false]".getBytes
    val fact = (new JsonFactory)
    val jax = fact.createNonBlockingByteArrayParser()
    val feeder = jax.getNonBlockingInputFeeder().asInstanceOf[ByteArrayFeeder]
    val spray = SprayParser.createJacksonWrapper()
    var elements = Nil: List[JsValue]
    
    input.foreach { byte =>
      feeder.feedInput(Array[Byte](byte), 0, 1)
      spray.parseArray(jax, consumer(v => elements ::= v))
    }
  
    feeder.endOfInput()
    spray.parseArray(jax, consumer(v => elements ::= v))
    
    assert(elements == JsFalse :: JsTrue :: JsNumber(1) :: Nil)
  }
  
  test("parseStream") {
    val input = "1 true\nfalse".getBytes
    val fact = (new JsonFactory)
    val jax = fact.createNonBlockingByteArrayParser()
    val feeder = jax.getNonBlockingInputFeeder().asInstanceOf[ByteArrayFeeder]
    val spray = SprayParser.createJacksonWrapper()
    var elements = Nil: List[JsValue]
    
    input.foreach { byte =>
      feeder.feedInput(Array[Byte](byte), 0, 1)
      spray.parseStream(jax, consumer(v => elements ::= v))
    }
    
    feeder.endOfInput()
    spray.parseStream(jax, consumer(v => elements ::= v))
    
    assert(elements == JsFalse :: JsTrue :: JsNumber(1) :: Nil)
  }
  
  test("Source[JsValue, NotUsed] read strict") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    implicit val config = JackshaftConfig.default.copy(streamingMode = StreamingMode.Array)

    val input = "[1, true, false]"
    val entity = Await.result(Marshal(input).to[MessageEntity], 1.seconds)
    val source = Await.result(Unmarshal(entity).to[Source[JsValue, NotUsed]], 1.seconds)

    val results = Await.result(source.runFold(Vector.empty[JsValue])(_ :+ _), 1.seconds)
    assert(results == Vector(JsNumber(1), JsTrue, JsFalse))
  }

  test("Source[JsValue, NotUsed] read chunked") {
    import akka.http.scaladsl.model.MediaTypes.`application/json`
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    implicit val config = JackshaftConfig.default.copy(streamingMode = StreamingMode.Array)

    val input = "[1, true, false]".getBytes
    // byte by byte
    val entity = HttpEntity.Chunked(`application/json`, Source(input.toVector.map { byte => HttpEntity.Chunk(ByteString(byte)) } :+ HttpEntity.LastChunk))
    val source = Await.result(Unmarshal(entity).to[Source[JsValue, NotUsed]], 1.seconds)

    val results = Await.result(source.runFold(Vector.empty[JsValue])(_ :+ _), 5.seconds)
    assert(results == Vector(JsNumber(1), JsTrue, JsFalse))
  }

  test("Source[JsValue, NotUsed] write") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    implicit val config = JackshaftConfig.default.copy(streamingMode = StreamingMode.Array)

    var input = Vector(JsNumber(1), JsTrue, JsFalse, JsString("foo"), JsObject("." -> JsArray.empty))
    (1 to 10).foreach { _ => input ++= input }
    val source = Source(input)
    // byte by byte
    val entity = Await.result(Marshal(source).to[MessageEntity], 2.seconds)
    val result = Await.result(Unmarshal(entity).to[JsValue], 2.seconds)

    assert(result == JsArray(input))
  }
  
  test("Source[JsValue, NotUsed] write big values") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    implicit val config = JackshaftConfig.default.copy(streamingMode = StreamingMode.Array)
    
    val bigValue = JsString("1234567890".repeat(500))
    
    val input = Vector(bigValue, bigValue, bigValue, bigValue)
    val source = Source(input)
    // byte by byte
    val entity = Await.result(Marshal(source).to[MessageEntity], 2.seconds)
    val result = Await.result(Unmarshal(entity).to[JsValue], 2.seconds)
    
    assert(result == JsArray(input))
  }
  
  test("JsValue chunked") {
    import akka.http.scaladsl.model.MediaTypes.`application/json`
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
  
    val input = "[1, true, false]".getBytes
    // byte by byte
    val entity = HttpEntity.Chunked(`application/json`, Source(input.toVector.map { byte => HttpEntity.Chunk(ByteString(byte)) } :+ HttpEntity.LastChunk))
    val result = Await.result(Unmarshal(entity).to[JsValue], 1.seconds)
    
    assert(result == JsArray(Vector(JsNumber(1), JsTrue, JsFalse)))
  }
  
  test("printer") {
    import spray.json._

    val input = "[1,{\"key\":[false,1.1,\"str\\u1234\",[null],[],{}]}]"
    val tree = input.parseJson

    assert(input == SprayPrinter.printString(tree))
  }
  
  test("ByteString marshaller") {
    import spray.json._
    import zygf.jackshaft.spray.AkkaSprayJsonSupport
    object Support extends AkkaSprayJsonSupport
    {
      implicit val bsMarshaller = toByteStringMarshaller 
    }
    import Support.bsMarshaller
    
    val input = "[1,{\"key\":[false,1.1,\"str\\u1234\",[null],[],{}]}]"
    val tree = input.parseJson
    
    assert(input == Await.result(Marshal(tree).to[ByteString], 1.seconds).decodeString("ascii"))
  }
  
  test("printer error") {
    import spray.json._
    
    val tree = JsArray(JsString(null: String))
  
    assertThrows[PrintingException](SprayPrinter.printString(tree))
  }
}

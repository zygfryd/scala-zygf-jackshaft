package zygf.tests

import java.util.function.Consumer

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{HttpEntity, MessageEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.core.async.ByteArrayFeeder
import spray.json._
import zygf.jackshaft.spray.SprayMiddleware

class SprayTests extends org.scalatest.FunSuite
{
  implicit val sys = ActorSystem("SprayJsonSupportSpec")
  implicit val mat = ActorMaterializer()
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
    val spray = new SprayMiddleware
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
    val spray = new SprayMiddleware
    var elements = Nil: List[JsValue]
    
    input.foreach { byte =>
      feeder.feedInput(Array[Byte](byte), 0, 1)
      spray.parseStream(jax, consumer(v => elements ::= v))
    }
    
    feeder.endOfInput()
    spray.parseStream(jax, consumer(v => elements ::= v))
    
    assert(elements == JsFalse :: JsTrue :: JsNumber(1) :: Nil)
  }
  
  test("Source[JsValue, NotUsed] strict") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
  
    val input = "[1, true, false]".getBytes
    val entity = Await.result(Marshal(input).to[MessageEntity], 1.seconds)
    val source = Await.result(Unmarshal(entity).to[Source[JsValue, NotUsed]], 1.seconds)
    
    val results = Await.result(source.runFold(Vector.empty[JsValue])(_ :+ _), 1.seconds)
    assert(results == Vector(JsNumber(1), JsTrue, JsFalse))
  }
  
  test("Source[JsValue, NotUsed] chunked") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    import akka.http.scaladsl.model.MediaTypes.`application/json`
  
    val input = "[1, true, false]".getBytes
    // byte by byte
    val entity = HttpEntity.Chunked(`application/json`, Source(input.toVector.map { byte => HttpEntity.Chunk(ByteString(byte)) } :+ HttpEntity.LastChunk))
    val source = Await.result(Unmarshal(entity).to[Source[JsValue, NotUsed]], 1.seconds)
    
    val results = Await.result(source.runFold(Vector.empty[JsValue])(_ :+ _), 5.seconds)
    assert(results == Vector(JsNumber(1), JsTrue, JsFalse))
  }
  
  test("JsValue chunked") {
    import zygf.jackshaft.spray.AkkaSprayJsonSupport._
    import akka.http.scaladsl.model.MediaTypes.`application/json`
  
    val input = "[1, true, false]".getBytes
    // byte by byte
    val entity = HttpEntity.Chunked(`application/json`, Source(input.toVector.map { byte => HttpEntity.Chunk(ByteString(byte)) } :+ HttpEntity.LastChunk))
    val result = Await.result(Unmarshal(entity).to[JsValue], 1.seconds)
    
    assert(result == JsArray(Vector(JsNumber(1), JsTrue, JsFalse)))
  }
}

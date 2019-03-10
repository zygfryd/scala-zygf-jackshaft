package zygf.jackshaft.impl

import scala.reflect.ClassTag

import zygf.jackshaft.conf.JackshaftConfig

abstract class ParsingMiddlewareImpl[J, A, M](val jClass: Class[J]) extends ParsingMiddleware[J]
{
  def this()(implicit J: ClassTag[J]) = this(J.runtimeClass.asInstanceOf[Class[J]])
  
  /**
    * Output an AST null node
    */
  def buildNull(): J
  
  /**
    * Output an AST true node
    */
  def buildTrue(): J
  
  /**
    * Output an AST false node
    */
  def buildFalse(): J
  
  /**
    * Output a number AST node
    */
  def buildNumber(num: Number): J
  
  /**
    * Output a string AST node
    */
  def buildString(text: String): J
  
  /**
    * Create an array AST node from a collection returned by `emptyArray()`
    */
  def buildArray(array: A): J
  
  /**
    * Output an empty array AST node
    */
  def buildArray(): J = buildArray(emptyArray())
  
  /**
    * Create an empty collection for storing array members
    */
  def emptyArray(): A
  
  /**
    * Add a member to a collection returned by `emptyArray()`
    */
  def growArray(array: A, value: J): A
  
  /**
    * Create an object AST node from a collection returned by `emptyMap()`
    */
  def buildObject(map: M): J
  
  /**
    * Output an empty object AST node
    */
  def buildObject(): J = buildObject(emptyMap())
  
  /**
    * Output a small object AST node with 1 to 4 members
    */
  def buildObject(keys: Array[String], vals: Array[J], at: Int, count: Int): J = {
    // unoptimized version
    var i = 0
    var map = emptyMap()
    while (i < count) {
      map = growMap(map, keys(i), vals(i))
      i += 1
    }
    buildObject(map)
  }
  
  /**
    * Create an empty collection for storing a bigger object's members
    */
  def emptyMap(): M
  
  /**
    * Add a member to a collection returned by `emptyMap()`
    */
  def growMap(map: M, key: String, value: J): M
  
  override def parseString(input: String)(implicit config: JackshaftConfig = JackshaftConfig.default) = {
    val jax = config.jacksonFactory.createParser(input)
    val wrapper = createJacksonWrapper()
    try wrapper.parseValue(jax)
    finally {
      jax.close()
    }
  }
  
  override def parseBytes(input: Array[Byte])(implicit config: JackshaftConfig = JackshaftConfig.default) = {
    val jax = config.jacksonFactory.createParser(input)
    val wrapper = createJacksonWrapper()
    try wrapper.parseValue(jax)
    finally {
      jax.close()
    }
  }
  
  final override def createJacksonWrapper()(implicit config: JackshaftConfig = JackshaftConfig.default): JacksonWrapper[J] =
    new JacksonWrapperImpl[J, A, M](this, jClass, 8, config.maxParsingDepth)
}

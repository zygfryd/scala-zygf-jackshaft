package zygf.jackshaft.impl

import scala.collection.immutable.TreeMap
import scala.reflect.ClassTag

abstract class OptimizedMapParsingMiddleware[J, A](implicit J: ClassTag[J]) extends ParsingMiddlewareImpl[J, A, Map[String, J]]
{
  final override def buildObject(keys: Array[String], vals: Array[J], at: Int, count: Int): J = {
    import scala.collection.immutable.Map._
    
    count match {
      case 1 => buildObject(new Map1(keys(at),     vals(at)))
      case 2 => buildObject(new Map2(keys(at),     vals(at),
                                     keys(at + 1), vals(at + 1)))
      case 3 => buildObject(new Map3(keys(at),     vals(at),
                                     keys(at + 1), vals(at + 1),
                                     keys(at + 2), vals(at + 2)))
      case 4 => buildObject(new Map4(keys(at),     vals(at),
                                     keys(at + 1), vals(at + 1),
                                     keys(at + 2), vals(at + 2),
                                     keys(at + 3), vals(at + 3)))
      case _ =>
        ???
    }
  }
  
  override def emptyMap() = TreeMap.empty[String, J]
  
  final override def growMap(map: Map[String, J], key: String, value: J) = map.updated(key, value)
}

package zygf.jackshaft.impl.akka

import akka.util.ByteString
import zygf.jackshaft.impl.WrapsByteArray

object Implicits
{
  implicit val byteStringWrapsByteArray: WrapsByteArray[ByteString] = new WrapsByteArray[ByteString] {
    override def copy(array: Array[Byte], offset: Int, length: Int) = ByteString.fromArray(array, offset, length)
    override def wrap(array: Array[Byte]) = ByteString.fromArrayUnsafe(array)
  }
}

package zygf.jackshaft.impl

abstract class WrapsByteArray[T >: Null]
{
  def copy(array: Array[Byte], offset: Int, length: Int): T
  def wrap(array: Array[Byte]) = copy(array, 0, array.length)
}

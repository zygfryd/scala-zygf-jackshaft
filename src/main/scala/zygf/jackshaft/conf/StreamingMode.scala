package zygf.jackshaft.conf

import zygf.jackshaft.impl.ParsingMode

sealed abstract class StreamingMode(val parsingMode: ParsingMode,
                                    val separator: Char) extends Product with Serializable

object StreamingMode
{
  case object Array extends StreamingMode(ParsingMode.ARRAY, ',')
  
  case object Whitespace extends StreamingMode(ParsingMode.STREAM, '\n')
}

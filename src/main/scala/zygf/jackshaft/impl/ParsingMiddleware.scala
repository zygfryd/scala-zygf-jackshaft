package zygf.jackshaft.impl

import zygf.jackshaft.conf.JackshaftConfig

abstract class ParsingMiddleware[J]
{
  def parseString(input: String)(implicit config: JackshaftConfig): J
  def parseBytes(input: Array[Byte])(implicit config: JackshaftConfig): J
  
  def createJacksonWrapper()(implicit config: JackshaftConfig): JacksonWrapper[J]
}



package zygf.jackshaft.conf

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder}

case class JackshaftConfig(maxParsingDepth:    Int = 64,
                           streamingMode:      StreamingMode = StreamingMode.Whitespace,
                           tempBufferProvider: TempBufferProvider = TempBufferProvider.ThreadLocal,
                           jacksonFactory:     JsonFactory = JackshaftConfig.defaultFactory)

object JackshaftConfig
{
  val defaultFactory = (new JsonFactoryBuilder)
    .enable(JsonFactory.Feature.CANONICALIZE_FIELD_NAMES)
    .disable(JsonFactory.Feature.INTERN_FIELD_NAMES)
    .build()
  
  val noThreadLocalsFactory = (new JsonFactoryBuilder)
    .enable(JsonFactory.Feature.CANONICALIZE_FIELD_NAMES)
    .disable(JsonFactory.Feature.INTERN_FIELD_NAMES)
    .disable(JsonFactory.Feature.USE_THREAD_LOCAL_FOR_BUFFER_RECYCLING)
    .build()
  
  implicit val default = JackshaftConfig()
  val noThreadLocals = default.copy(tempBufferProvider = TempBufferProvider.Fresh,
                                    jacksonFactory = noThreadLocalsFactory)
}

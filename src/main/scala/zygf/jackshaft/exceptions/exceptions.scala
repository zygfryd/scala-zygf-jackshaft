package zygf.jackshaft.exceptions

case class UnexpectedEndOfInputException(message: String) extends RuntimeException(message, null, true, false)

object UnexpectedEndOfInputException extends UnexpectedEndOfInputException("Unexpected end of JSON document")

case class ArrayExpectedException(message: String) extends RuntimeException(message, null, true, false)

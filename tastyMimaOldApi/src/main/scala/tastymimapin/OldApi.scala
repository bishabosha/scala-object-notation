package tastymimapin

import scalanotation.ReadWriter

final case class IntValue(value: Int)
final case class LongValue(value: Long)
final case class FloatValue(value: Float)
final case class DoubleValue(value: Double)

object OldApi:
  inline def intReadWriter: ReadWriter[IntValue] =
    ReadWriter.int[IntValue](IntValue(_))(_.value)

  inline def longReadWriter: ReadWriter[LongValue] =
    ReadWriter.long[LongValue](LongValue(_))(_.value)

  inline def floatReadWriter: ReadWriter[FloatValue] =
    ReadWriter.float[FloatValue](FloatValue(_))(_.value)

  inline def doubleReadWriter: ReadWriter[DoubleValue] =
    ReadWriter.double[DoubleValue](DoubleValue(_))(_.value)

package tastymimapin

/** Expanding these inline methods against current core proves that @publicInBinary keeps the old
  * public overloads available to TASTy compiled against 0.4.0, despite their current source
  * visibility being private[scalanotation].
  */
object Reinline:
  val intReadWriter    = OldApi.intReadWriter
  val longReadWriter   = OldApi.longReadWriter
  val floatReadWriter  = OldApi.floatReadWriter
  val doubleReadWriter = OldApi.doubleReadWriter

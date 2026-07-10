package scalanotation.schema

import scalanotation.DecodeError
import scalanotation.RouterSchema
import scalanotation.{Reader, Writer}
import steps.result.Result

/** Router schemas for the arbitrary-precision number types: a value reads from either a string
  * literal or a number literal within Scala's bounded literal syntax, and always writes as a
  * string. Values beyond the bounded literals' range or precision spell as strings.
  */
private[scalanotation] object BigNumberSchemas:

  val BigIntSchema: RawSchema[BigInt] =
    inline val StringCase  = 0
    inline val IntegerCase = 1
    RawSchema.Router(
      name = "BigInt",
      selfKind = "BigInt",
      IArray(
        RawSchema.RouterCase("String", stringCase("BigInt")(BigInt(_), _.toString)),
        // the Long decoder accepts int literals too, so one case serves both integer constructs
        RawSchema.RouterCase("Integer", integerCase[BigInt](BigInt(_), _.toLong))
      ),
      RouterSchema.Router(
        record = RawSchema.UnsupportedRouterCase,
        tuple = RawSchema.UnsupportedRouterCase,
        vector = RawSchema.UnsupportedRouterCase,
        string = StringCase,
        char = RawSchema.UnsupportedRouterCase,
        int = IntegerCase,
        long = IntegerCase,
        float = RawSchema.UnsupportedRouterCase,
        double = RawSchema.UnsupportedRouterCase,
        boolean = RawSchema.UnsupportedRouterCase,
        `null` = RawSchema.UnsupportedRouterCase,
        rawNumber = RawSchema.UnsupportedRouterCase,
        unsupported = RawSchema.UnsupportedRouterCase
      ),
      WriteAsString,
      RouterSchema.NumberMode.Bounded
    )

  val BigDecimalSchema: RawSchema[BigDecimal] =
    inline val StringCase  = 0
    inline val IntegerCase = 1
    inline val FloatCase   = 2
    inline val DoubleCase  = 3
    RawSchema.Router(
      name = "BigDecimal",
      selfKind = "BigDecimal",
      IArray(
        RawSchema.RouterCase("String", stringCase("BigDecimal")(BigDecimal(_), _.toString)),
        RawSchema.RouterCase("Integer", integerCase[BigDecimal](BigDecimal(_), _.toLong)),
        RawSchema.RouterCase("Float", floatCase[BigDecimal](BigDecimal.decimal(_), _.toFloat)),
        RawSchema.RouterCase("Double", doubleCase[BigDecimal](BigDecimal.decimal(_), _.toDouble))
      ),
      RouterSchema.Router(
        record = RawSchema.UnsupportedRouterCase,
        tuple = RawSchema.UnsupportedRouterCase,
        vector = RawSchema.UnsupportedRouterCase,
        string = StringCase,
        char = RawSchema.UnsupportedRouterCase,
        int = IntegerCase,
        long = IntegerCase,
        float = FloatCase,
        double = DoubleCase,
        boolean = RawSchema.UnsupportedRouterCase,
        `null` = RawSchema.UnsupportedRouterCase,
        rawNumber = RawSchema.UnsupportedRouterCase,
        unsupported = RawSchema.UnsupportedRouterCase
      ),
      WriteAsString,
      RouterSchema.NumberMode.Bounded
    )

  private object WriteAsString extends RouterSchema.Write[Any]:
    def caseIndex(router: RouterSchema.Router, value: Any): RouterSchema.Index =
      router.stringIndex

  private def stringCase[A](typeName: String)(
      parse: String => A,
      write: A => String
  ): RawSchema[A] =
    RawSchema.mapResultAndInput(RawSchema.String)(
      resultMap0 = raw =>
        Result.catchException({ case _: NumberFormatException =>
          DecodeError.Custom(s"Invalid $typeName '$raw'")
        }) {
          parse(raw)
        },
      inputMap0 = value => write(value)
    )

  private def integerCase[A](
      fromLong: Reader.LongMap[A],
      toLong: Writer.LongMap[A]
  ): RawSchema[A] =
    RawSchema.mapLongTotalAndInput(RawSchema.Long)(
      resultMap0 = fromLong,
      // the write side always selects the string case, so the input maps stay unreached
      inputMap0 = toLong
    )

  private def floatCase[A](
      fromFloat: Reader.FloatMap[A],
      toFloat: Writer.FloatMap[A]
  ): RawSchema[A] =
    RawSchema.mapFloatTotalAndInput(RawSchema.Float)(
      resultMap0 = fromFloat,
      inputMap0 = toFloat
    )

  private def doubleCase[A](
      fromDouble: Reader.DoubleMap[A],
      toDouble: Writer.DoubleMap[A]
  ): RawSchema[A] =
    RawSchema.mapDoubleTotalAndInput(RawSchema.Double)(
      resultMap0 = fromDouble,
      inputMap0 = toDouble
    )

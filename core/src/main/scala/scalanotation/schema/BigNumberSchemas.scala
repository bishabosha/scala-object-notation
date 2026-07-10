package scalanotation.schema

import scalanotation.DecodeError
import scalanotation.RouterSchema
import steps.result.Result

/** Router schemas for the arbitrary-precision number types: a value reads from either a string
  * literal or any number literal (routed raw under [[RouterSchema.NumberMode.Raw]], so no range or
  * precision is lost), and always writes as a string.
  */
private[scalanotation] object BigNumberSchemas:

  val BigIntSchema: RawSchema[BigInt] =
    bigNumberRouter[BigInt]("BigInt")(
      fromString = BigInt(_),
      fromNumber = parseRawBigInt,
      write = _.toString
    )

  val BigDecimalSchema: RawSchema[BigDecimal] =
    bigNumberRouter[BigDecimal]("BigDecimal")(
      fromString = BigDecimal(_),
      fromNumber = parseRawBigDecimal,
      write = _.toString
    )

  private def bigNumberRouter[A](name: String)(
      fromString: String => A,
      fromNumber: String => A,
      write: A => String
  ): RawSchema[A] =
    inline val StringCase = 0
    inline val NumberCase = 1
    RawSchema.Router(
      name = name,
      selfKind = name,
      IArray(
        RawSchema.RouterCase("String", mappedParse(RawSchema.String, name)(fromString, write)),
        RawSchema.RouterCase("Number", mappedParse(RawSchema.RawNumber, name)(fromNumber, write))
      ),
      RouterSchema.Router(
        record = RawSchema.UnsupportedRouterCase,
        tuple = RawSchema.UnsupportedRouterCase,
        vector = RawSchema.UnsupportedRouterCase,
        string = StringCase,
        char = RawSchema.UnsupportedRouterCase,
        int = RawSchema.UnsupportedRouterCase,
        long = RawSchema.UnsupportedRouterCase,
        float = RawSchema.UnsupportedRouterCase,
        double = RawSchema.UnsupportedRouterCase,
        boolean = RawSchema.UnsupportedRouterCase,
        `null` = RawSchema.UnsupportedRouterCase,
        rawNumber = NumberCase,
        unsupported = RawSchema.UnsupportedRouterCase
      ),
      WriteAsString,
      RouterSchema.NumberMode.Raw
    )

  private object WriteAsString extends RouterSchema.Write[Any]:
    def caseIndex(router: RouterSchema.Router, value: Any): RouterSchema.Index =
      router.stringIndex

  private def mappedParse[A](base: RawSchema[String], typeName: String)(
      parse: String => A,
      write: A => String
  ): RawSchema[A] =
    RawSchema.mapResultAndInput(base)(
      resultMap0 = raw =>
        Result.catchException({ case _: NumberFormatException =>
          DecodeError.Custom(s"Invalid $typeName '$raw'")
        }) {
          parse(raw)
        },
      inputMap0 = value => write(value.asInstanceOf[A])
    )

  // The raw-number parses accept everything the tokenizer classifies as a number literal —
  // including the `0x`/`0b` prefixed forms, which the plain constructors reject.

  private def parseRawBigInt(raw: String): BigInt =
    val negative = raw.charAt(0) == '-'
    val start    = if negative then 1 else 0
    val radix    = prefixedRadix(raw, start)
    if radix == 0 then BigInt(raw)
    else
      val magnitude = BigInt(raw.substring(start + 2), radix)
      if negative then -magnitude else magnitude

  private def parseRawBigDecimal(raw: String): BigDecimal =
    val negative = raw.charAt(0) == '-'
    val start    = if negative then 1 else 0
    val radix    = prefixedRadix(raw, start)
    if radix == 0 then BigDecimal(raw)
    else
      val magnitude = BigDecimal(BigInt(raw.substring(start + 2), radix))
      if negative then -magnitude else magnitude

  /** 16 or 2 for a `0x`/`0b` prefix at `start`, 0 for a plain decimal literal */
  private def prefixedRadix(raw: String, start: Int): Int =
    if raw.length > start + 1 && raw.charAt(start) == '0' then
      raw.charAt(start + 1) match
        case 'x' | 'X' => 16
        case 'b' | 'B' => 2
        case _         => 0
    else 0

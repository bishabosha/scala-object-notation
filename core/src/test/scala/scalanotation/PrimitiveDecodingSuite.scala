package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import scalanotation.schema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class PrimitiveDecodingSuite extends ScalanotationSuite:
  test("decode just expression"):
    type Data = (a: Boolean, b: Boolean, c: Int, d: Float)
    val input  = "(a = true, b = false, c = -12, d = -1.5f)"
    val parsed = Readers.readAs[Data](input)
    val data   = parsed.getOrElse(fail(s"Expected successful parse, got $parsed"))
    assertEquals(data, (a = true, b = false, c = -12, d = -1.5f))

  test("decode lossless numeric literal promotions in read mode"):
    type Data =
      (
          intToLong: Long,
          intToDouble: Double,
          intToFloat: Float
      )

    val input =
      """(
        |  intToLong = 1,
        |  intToDouble = 2,
        |  intToFloat = 16_777_216
        |)
        |""".stripMargin

    val expected: Data =
      (
        intToLong = 1L,
        intToDouble = 2.0d,
        intToFloat = 16_777_216.0f
      )

    assertEquals(Readers.readAs[Data](input), Result.Ok(expected))

  test("round-trip int literal promotions for float and double through declaration text"):
    type Data =
      (
          intToFloat: Float,
          intToDouble: Double
      )

    val input =
      """val data = (
        |  intToFloat = 16_777_216,
        |  intToDouble = 2_147_483_647
        |)
        |""".stripMargin

    val expected: Data =
      (
        intToFloat = 16_777_216.0f,
        intToDouble = 2_147_483_647.0d
      )

    val decoded  = Readers.readDeclAs[Data](input, rootName = "data")
    val value    = decoded.getOrElse(fail(s"Expected successful parse, got $decoded"))
    val rendered = Writers.writeDecl("data", value)
    val reparsed = Readers.readDeclAs[Data](rendered, rootName = "data")

    assertEquals(value, expected)
    assertEquals(reparsed, Result.Ok(expected))

  test("decode integer literals at the representable bounds"):
    assertEquals(Readers.readAs[Int]("2_147_483_647"), Result.Ok(Int.MaxValue))
    assertEquals(Readers.readAs[Int]("-2_147_483_648"), Result.Ok(Int.MinValue))
    assertEquals(Readers.readAs[Int]("0x7FFF_FFFF"), Result.Ok(Int.MaxValue))
    assertEquals(Readers.readAs[Int]("-0x8000_0000"), Result.Ok(Int.MinValue))
    assertEquals(Readers.readAs[Long]("9_223_372_036_854_775_807L"), Result.Ok(Long.MaxValue))
    assertEquals(Readers.readAs[Long]("-9_223_372_036_854_775_808L"), Result.Ok(Long.MinValue))
    assertEquals(Readers.readAs[Long]("0x7FFF_FFFF_FFFF_FFFFL"), Result.Ok(Long.MaxValue))
    assertEquals(Readers.readAs[Long]("-0x8000_0000_0000_0000L"), Result.Ok(Long.MinValue))
    assertEquals(Readers.readAs[Double]("-2_147_483_648"), Result.Ok(Int.MinValue.toDouble))
    assertEquals(Readers.readAs[Float]("-2_147_483_648"), Result.Ok(Int.MinValue.toFloat))

  test("public primitive reader constructors map from primitive slots"):
    final case class IntLiteral(value: Int)
    final case class LongLiteral(value: Long)
    final case class FloatLiteral(value: Float)
    final case class DoubleLiteral(value: Double)

    def assertTotalMap[T](reader: Reader[T], base: RawSchema[?])(
        isExpected: schema.SchemaMapping.TotalMap[?, ?] => Boolean
    ): Unit =
      reader.schema match
        case RawSchema.Mapped(mappedBase, mapping) if mappedBase == base =>
          assert(mapping.resultMap == null)
          assert(isExpected(mapping.totalMaps), s"Unexpected total map: ${mapping.totalMaps}")
        case other =>
          fail(s"Expected a mapped ${base.describeSelf} schema, got ${other.describeSelf}")

    var calls                = 0
    given Reader[IntLiteral] = Reader.int: value =>
      calls += 1
      IntLiteral(value)

    assertTotalMap(summon[Reader[IntLiteral]], RawSchema.Int):
      case schema.SchemaMapping.TotalMap.IntMap(_) => true
      case _                                       => false
    assertEquals(Readers.readAs[IntLiteral]("123"), Result.Ok(IntLiteral(123)))
    assertEquals(Expr.IntConstant(456).decodeAs[IntLiteral], Result.Ok(IntLiteral(456)))
    assertEquals(calls, 2)

    var longCalls             = 0
    given Reader[LongLiteral] = Reader.long: value =>
      longCalls += 1
      LongLiteral(value)

    assertTotalMap(summon[Reader[LongLiteral]], RawSchema.Long):
      case schema.SchemaMapping.TotalMap.LongMap(_) => true
      case _                                        => false
    assertEquals(Readers.readAs[LongLiteral]("123L"), Result.Ok(LongLiteral(123L)))
    assertEquals(Expr.LongConstant(456L).decodeAs[LongLiteral], Result.Ok(LongLiteral(456L)))
    assertEquals(longCalls, 2)

    var floatCalls             = 0
    given Reader[FloatLiteral] = Reader.float: value =>
      floatCalls += 1
      FloatLiteral(value)

    assertTotalMap(summon[Reader[FloatLiteral]], RawSchema.Float):
      case schema.SchemaMapping.TotalMap.FloatMap(_) => true
      case _                                         => false
    assertEquals(Readers.readAs[FloatLiteral]("1.5f"), Result.Ok(FloatLiteral(1.5f)))
    assertEquals(Expr.FloatConstant(2.5f).decodeAs[FloatLiteral], Result.Ok(FloatLiteral(2.5f)))
    assertEquals(floatCalls, 2)

    var doubleCalls             = 0
    given Reader[DoubleLiteral] = Reader.double: value =>
      doubleCalls += 1
      DoubleLiteral(value)

    assertTotalMap(summon[Reader[DoubleLiteral]], RawSchema.Double):
      case schema.SchemaMapping.TotalMap.DoubleMap(_) => true
      case _                                          => false
    assertEquals(Readers.readAs[DoubleLiteral]("1.25"), Result.Ok(DoubleLiteral(1.25d)))
    assertEquals(Expr.DoubleConstant(2.5d).decodeAs[DoubleLiteral], Result.Ok(DoubleLiteral(2.5d)))
    assertEquals(doubleCalls, 2)

  test("reject integer literals that overflow their type"):
    def assertTokenFormat[T: Reader](input: String, expected: String): Unit =
      Readers.readAs[T](input) match
        case Result.Err(error) => assertEquals(error.rootCause, DecodeError.TokenFormat(expected))
        case Result.Ok(value)  => fail(s"Expected a decode failure, got $value")

    assertTokenFormat[Int]("2147483648", "Invalid Int literal '2147483648'")
    assertTokenFormat[Int]("-2147483649", "Invalid Int literal '2147483649'")
    assertTokenFormat[Int]("0x8000_0000", "Invalid Int literal '0x8000_0000'")
    assertTokenFormat[Int]("-0x8000_0001", "Invalid Int literal '0x8000_0001'")
    assertTokenFormat[Long](
      "9223372036854775808L",
      "Invalid Long literal '9223372036854775808L'"
    )
    assertTokenFormat[Long](
      "-9223372036854775809L",
      "Invalid Long literal '9223372036854775809L'"
    )

  test("round-trip the extreme values of every numeric type"):
    def assertRoundTrip[T: {Reader, Writer}](value: T): Unit =
      assertEquals(Readers.readAs[T](Writers.write(value)), Result.Ok(value))

    assertRoundTrip(Int.MinValue)
    assertRoundTrip(Int.MaxValue)
    assertRoundTrip(Long.MinValue)
    assertRoundTrip(Long.MaxValue)
    assertRoundTrip(Float.MinValue)
    assertRoundTrip(Float.MaxValue)
    assertRoundTrip(java.lang.Float.MIN_VALUE) // smallest positive subnormal
    assertRoundTrip(Double.MinValue)
    assertRoundTrip(Double.MaxValue)
    assertRoundTrip(java.lang.Double.MIN_VALUE)

  test("reject numeric literal promotions that would lose precision"):
    val floatErr         = Readers.readAs[Float]("16_777_217")
    val doubleToFloatErr = Readers.readAs[Float]("0.1")
    val longToFloatErr   = Readers.readAs[Float]("33_554_432L")
    val longToDoubleErr  = Readers.readAs[Double]("9L")
    val floatToDoubleErr = Readers.readAs[Double]("3.5f")

    floatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "integer literal '16_777_217'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    doubleToFloatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "double literal '0.1'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    longToFloatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "long literal '33_554_432L'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    longToDoubleErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Double", "long literal '9L'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    floatToDoubleErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Double", "float literal '3.5f'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode lossless numeric literal promotions from Expr values"):
    val expr = Expr.NamedTupleExpr(
      IndexedSeq(
        "intToLong"   -> Expr.IntConstant(1),
        "intToDouble" -> Expr.IntConstant(2),
        "intToFloat"  -> Expr.IntConstant(16_777_216)
      )
    )

    type Data =
      (
          intToLong: Long,
          intToDouble: Double,
          intToFloat: Float
      )

    val expected: Data =
      (
        intToLong = 1L,
        intToDouble = 2.0d,
        intToFloat = 16_777_216.0f
      )

    assertEquals(expr.decodeAs[Data], Result.Ok(expected))
    assertEquals(
      Expr.DoubleConstant(0.1d).decodeAs[Float],
      Result.Err(DecodeError.ExpectedType("Float", "(0.1: Double)"))
    )
    assertEquals(
      Expr.LongConstant(9L).decodeAs[Double],
      Result.Err(DecodeError.ExpectedType("Double", "(9: Long)"))
    )
    assertEquals(
      Expr.FloatConstant(3.5f).decodeAs[Double],
      Result.Err(DecodeError.ExpectedType("Double", "(3.5: Float)"))
    )

  test("BigInt and BigDecimal map through String instances"):
    val bigIntValue     = BigInt("123456789012345678901234567890")
    val bigDecimalValue = BigDecimal("1234567890.012345678900")

    assertEquals(summon[Reader[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[Writer[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[ReadWriter[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[Reader[BigDecimal]].schema.describeSelf, "String")
    assertEquals(summon[Writer[BigDecimal]].schema.describeSelf, "String")
    assertEquals(summon[ReadWriter[BigDecimal]].schema.describeSelf, "String")

    assertEquals(Readers.readAs[BigInt](s""""$bigIntValue""""), Result.Ok(bigIntValue))
    assertEquals(Readers.readAs[BigDecimal](s""""$bigDecimalValue""""), Result.Ok(bigDecimalValue))
    assertEquals(Writers.write(bigIntValue), s""""$bigIntValue"""")
    assertEquals(Writers.write(bigDecimalValue), s""""$bigDecimalValue"""")

    type Data = (count: BigInt, amount: BigDecimal)
    val value: Data =
      (
        count = bigIntValue,
        amount = bigDecimalValue
      )

    val rendered = Writers.write(value)
    assertEquals(rendered, s"""(count = "$bigIntValue", amount = "$bigDecimalValue")""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

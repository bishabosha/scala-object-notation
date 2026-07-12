package scalanotation.json

import steps.result.Result

import java.nio.charset.StandardCharsets

class JsonPrimitiveSuite extends munit.FunSuite:

  private def ok[T](result: Result[T, scalanotation.DecodeError]): T =
    result.getOrElse(fail(s"Expected successful parse, got $result"))

  test("decode primitives"):
    assertEquals(Json.readAs[Int]("42"), Result.Ok(42))
    assertEquals(Json.readAs[Int]("-42"), Result.Ok(-42))
    assertEquals(Json.readAs[Int]("0"), Result.Ok(0))
    assertEquals(Json.readAs[Int]("-0"), Result.Ok(0))
    assertEquals(Json.readAs[Long]("9223372036854775807"), Result.Ok(Long.MaxValue))
    assertEquals(Json.readAs[Long]("-9223372036854775808"), Result.Ok(Long.MinValue))
    assertEquals(Json.readAs[Int]("2147483647"), Result.Ok(Int.MaxValue))
    assertEquals(Json.readAs[Int]("-2147483648"), Result.Ok(Int.MinValue))
    assertEquals(Json.readAs[Double]("1.5"), Result.Ok(1.5))
    assertEquals(Json.readAs[Double]("-1.5e10"), Result.Ok(-1.5e10))
    assertEquals(Json.readAs[Double]("3"), Result.Ok(3.0))
    assertEquals(Json.readAs[Float]("1.25"), Result.Ok(1.25f))
    assertEquals(Json.readAs[Boolean]("true"), Result.Ok(true))
    assertEquals(Json.readAs[Boolean]("false"), Result.Ok(false))
    assertEquals(Json.readAs[String]("\"hello\""), Result.Ok("hello"))
    assertEquals(Json.readAs[Char]("\"x\""), Result.Ok('x'))
    assertEquals(Json.readAs[Option[Int]]("null"), Result.Ok(None))
    assertEquals(Json.readAs[Option[Int]]("7"), Result.Ok(Some(7)))

  test("decode with surrounding whitespace"):
    assertEquals(Json.readAs[Int]("  42\n"), Result.Ok(42))
    assertEquals(Json.readAs[Boolean](" \t true \r\n"), Result.Ok(true))

  test("reject numbers that do not fit the schema type"):
    assert(Json.readAs[Int]("2147483648").isErr)
    assert(Json.readAs[Int]("-2147483649").isErr)
    assert(Json.readAs[Int]("1.5").isErr)
    assert(Json.readAs[Int]("1e2").isErr)
    assert(Json.readAs[Long]("9223372036854775808").isErr)
    assert(Json.readAs[Long]("-9223372036854775809").isErr)
    assert(Json.readAs[Long]("1.0").isErr)

  test("reject malformed numbers"):
    assert(Json.readAs[Int]("01").isErr)
    assert(Json.readAs[Int]("-").isErr)
    assert(Json.readAs[Double]("1.").isErr)
    assert(Json.readAs[Double](".5").isErr)
    assert(Json.readAs[Double]("1e").isErr)
    assert(Json.readAs[Double]("+1").isErr)
    assert(Json.readAs[Double]("NaN").isErr)

  test("reject type mismatches"):
    assert(Json.readAs[Int]("\"42\"").isErr)
    assert(Json.readAs[String]("42").isErr)
    assert(Json.readAs[Boolean]("1").isErr)
    assert(Json.readAs[Char]("\"ab\"").isErr)
    assert(Json.readAs[Char]("\"\"").isErr)

  test("reject trailing content"):
    assert(Json.readAs[Int]("42 43").isErr)
    assert(Json.readAs[Boolean]("true false").isErr)
    assert(Json.readAs[String]("\"a\" \"b\"").isErr)

  test("reject invalid literals"):
    assert(Json.readAs[Boolean]("tru").isErr)
    assert(Json.readAs[Boolean]("truthy").isErr)
    assert(Json.readAs[Option[Int]]("nul").isErr)

  test("decode string escapes"):
    assertEquals(
      Json.readAs[String]("\"a\\n\\t\\\"\\\\b\\/\""),
      Result.Ok("a\n\t\"\\b/")
    )
    assertEquals(Json.readAs[String]("\"\\u0041\\u00e9\""), Result.Ok("Aé"))
    // surrogate pair escapes
    assertEquals(Json.readAs[String]("\"\\ud83d\\ude00\""), Result.Ok("😀"))
    assertEquals(Json.readAs[Char]("\"\\n\""), Result.Ok('\n'))

  test("reject invalid strings"):
    assert(Json.readAs[String]("\"unterminated").isErr)
    assert(Json.readAs[String]("\"bad \\x escape\"").isErr)
    assert(Json.readAs[String](("\"ctrl " + "\u0001" + "\"")).isErr)
    assert(Json.readAs[String]("\"\\u12g4\"").isErr)

  test("decode non-ASCII strings"):
    assertEquals(Json.readAs[String]("\"héllo wörld\""), Result.Ok("héllo wörld"))
    assertEquals(Json.readAs[String]("\"日本語\""), Result.Ok("日本語"))
    assertEquals(Json.readAs[String]("\"emoji 😀 mixed\""), Result.Ok("emoji 😀 mixed"))

  test("byte input decodes identically to String input"):
    val inputs = List(
      "\"héllo 😀 wörld\"",
      "42",
      "-1.5e-3",
      "[1,2,3]",
      "{\"a\": \"日本語\"}"
    )
    for input <- inputs do
      val bytes = input.getBytes(StandardCharsets.UTF_8)
      assertEquals(
        Json.readAs[JsonValue](bytes),
        Json.readAs[JsonValue](input),
        clue = input
      )

  test("large String inputs beyond the pooled buffer decode correctly"):
    val big = (1 to 4000).map(i => s"\"s$i\"").mkString("[", ",", "]")
    assert(big.length > 16384)
    val decoded = ok(Json.readAs[Vector[String]](big))
    assertEquals(decoded.length, 4000)
    assertEquals(decoded.head, "s1")
    assertEquals(decoded.last, "s4000")

  test("BigInt and BigDecimal decode from numbers and strings"):
    assertEquals(Json.readAs[BigInt]("123"), Result.Ok(BigInt(123)))
    assertEquals(
      Json.readAs[BigInt]("\"123456789012345678901234567890\""),
      Result.Ok(BigInt("123456789012345678901234567890"))
    )
    assertEquals(Json.readAs[BigDecimal]("1.5"), Result.Ok(BigDecimal("1.5")))
    assertEquals(Json.readAs[BigDecimal]("\"1.5\""), Result.Ok(BigDecimal("1.5")))

  test("write primitives"):
    assertEquals(Json.write(42), "42")
    assertEquals(Json.write(-42L), "-42")
    assertEquals(Json.write(1.5), "1.5")
    assertEquals(Json.write(1.25f), "1.25")
    assertEquals(Json.write(true), "true")
    assertEquals(Json.write("hello"), "\"hello\"")
    assertEquals(Json.write('x'), "\"x\"")
    assertEquals(Json.write(Option.empty[Int]), "null")
    assertEquals(Json.write(Some(7): Option[Int]), "7")

  test("write string escapes"):
    assertEquals(Json.write("a\nb\t\"c\"\\"), "\"a\\nb\\t\\\"c\\\"\\\\\"")
    assertEquals(Json.write("\u0001"), "\"\\u0001\"")
    assertEquals(Json.write("日本語 😀"), "\"日本語 😀\"")

  test("writing non-finite floating point fails"):
    intercept[IllegalArgumentException](Json.write(Double.NaN))
    intercept[IllegalArgumentException](Json.write(Double.PositiveInfinity))
    intercept[IllegalArgumentException](Json.write(Float.NegativeInfinity))

  test("primitive round trips"):
    def roundTrip[T: scalanotation.ReadWriter](value: T): Unit =
      assertEquals(Json.readAs[T](Json.write(value)), Result.Ok(value))
    roundTrip(42)
    roundTrip(Long.MinValue)
    roundTrip(1.5e-300)
    roundTrip("with \"quotes\" and \\ and 日本語")
    roundTrip(Option.empty[String])

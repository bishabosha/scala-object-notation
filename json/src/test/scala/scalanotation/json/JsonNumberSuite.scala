package scalanotation.json

import steps.result.Result

/** Locks the exact-conversion fast paths to bit-for-bit parity with the JDK parsers. */
class JsonNumberSuite extends munit.FunSuite:

  private def parsedDouble(text: String): Double =
    Json.readAs[Double](text).getOrElse(fail(s"failed to parse $text"))

  private def parsedFloat(text: String): Float =
    Json.readAs[Float](text).getOrElse(fail(s"failed to parse $text"))

  private def assertDoubleParity(text: String): Unit =
    val expected = java.lang.Double.parseDouble(text)
    val actual   = parsedDouble(text)
    assertEquals(
      java.lang.Double.doubleToRawLongBits(actual),
      java.lang.Double.doubleToRawLongBits(expected),
      clue = s"$text: got $actual, expected $expected"
    )

  private def assertFloatParity(text: String): Unit =
    val expected = java.lang.Float.parseFloat(text)
    val actual   = parsedFloat(text)
    assertEquals(
      java.lang.Float.floatToRawIntBits(actual),
      java.lang.Float.floatToRawIntBits(expected),
      clue = s"$text: got $actual, expected $expected"
    )

  test("double parity on a fixed corpus"):
    val corpus = List(
      "0",
      "-0",
      "0.0",
      "-0.0",
      "1",
      "1.0",
      "1.5",
      "-1.5",
      "9007199254740992",
      "9007199254740993",
      "9007199254740991.5",
      "1e22",
      "1e-22",
      "1e23",
      "1e-23",
      "2.2250738585072014e-308",
      "1.7976931348623157e308",
      "4.9e-324",
      "2.2250738585072011e-308",
      "0.1",
      "0.2",
      "0.3",
      "0.7",
      "123.456e-7",
      "3.141592653589793",
      "100.99",
      "12345.6789",
      "0.000001",
      "1000000.000001",
      "18014398509481984",
      "36028797018963968",
      "0.00000000000000000000001",
      "1e308",
      "1e-308",
      "9.999999999999999e22",
      "123456789012345678901234567890.5"
    )
    corpus.foreach(assertDoubleParity)

  test("float parity on a fixed corpus"):
    val corpus = List(
      "0",
      "-0",
      "0.0",
      "1",
      "1.5",
      "-1.25",
      "16777216",
      "16777217",
      "1e10",
      "1e-10",
      "1e11",
      "1e-11",
      "3.4028235e38",
      "1.4e-45",
      "0.1",
      "0.7",
      "100.99",
      "1.17549435e-38",
      "6.7108864e7"
    )
    corpus.foreach(assertFloatParity)

  test("double parity on random values"):
    val random = new scala.util.Random(20260712)
    var i      = 0
    while i < 20000 do
      val value = random.nextInt(6) match
        case 0 => random.nextLong().toString
        case 1 => random.nextDouble().toString
        case 2 => (random.nextDouble() * math.pow(10, random.nextInt(40) - 20)).toString
        case 3 =>
          java.lang.Double.longBitsToDouble(random.nextLong()) match
            case d if java.lang.Double.isFinite(d) => d.toString
            case _                                 => random.nextInt().toString
        case 4 => s"${random.nextInt(1000)}.${random.nextInt(1000000000)}"
        case 5 => s"${random.nextInt(100)}.${random.nextInt(1000)}e${random.nextInt(45) - 22}"
      // normalize to a valid JSON number spelling (no '+' exponents from toString are produced
      // by Double.toString, and Scala never emits leading '+', so values are JSON-legal)
      assertDoubleParity(value)
      i += 1

  test("float parity on random values"):
    val random = new scala.util.Random(20260713)
    var i      = 0
    while i < 20000 do
      val value = random.nextInt(4) match
        case 0 => random.nextInt().toString
        case 1 => random.nextFloat().toString
        case 2 =>
          java.lang.Float.intBitsToFloat(random.nextInt()) match
            case f if java.lang.Float.isFinite(f) => f.toString
            case _                                => random.nextInt(1000).toString
        case 3 => s"${random.nextInt(1000)}.${random.nextInt(100000)}e${random.nextInt(21) - 10}"
      assertFloatParity(value)
      i += 1

  test("long boundary digits"):
    assertEquals(Json.readAs[Long]("999999999999999999"), Result.Ok(999999999999999999L))
    assertEquals(Json.readAs[Long]("1000000000000000000"), Result.Ok(1000000000000000000L))
    assertEquals(Json.readAs[Long]("-999999999999999999"), Result.Ok(-999999999999999999L))
    assertEquals(Json.readAs[Long]("-1000000000000000000"), Result.Ok(-1000000000000000000L))
    assert(Json.readAs[Long]("99999999999999999999").isErr)

  test("many-digit doubles fall back to exact parsing"):
    assertDoubleParity("3.141592653589793238462643383279502884197169399375105820974944")
    assertDoubleParity("0.000000000000000000000000000000000000000000001")
    assertDoubleParity("123456789012345678901234567890123456789")

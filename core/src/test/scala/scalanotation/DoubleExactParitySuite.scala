package scalanotation

/** The scanner's exact power-of-ten double fast path must agree bit-for-bit with
  * `Double.parseDouble` on every literal it accepts (both operands exactly representable, one IEEE
  * rounding); everything outside its bounds falls back to parseDouble itself.
  */
class DoubleExactParitySuite extends munit.FunSuite:
  private def decoded(literal: String): Double =
    Readers.readAs[(x: Double)](s"(x = $literal)") match
      case steps.result.Result.Ok(value)  => value.x
      case steps.result.Result.Err(error) => fail(error.format)

  test("fast-path literals agree bit-for-bit with parseDouble"):
    val literals = Seq(
      "0.0",
      "0.5",
      "1.5",
      "12345.99",
      "99999999.99",
      "0.1",
      "0.3",
      "2.675",
      "1e22",
      "1e-22",
      "1.7976931348623157e22",
      "9007199254740992.0",   // 2^53 exact
      "9007199254740993.0",   // 2^53 + 1: 17 digits, still <= 18 — must round like parseDouble
      "123456789012345678.0", // 18 digits at the accumulator limit
      "5d",
      "5.0d",
      "2.5e10",
      "3.25e-7",
      "0.000001",
      "1000000.000001"
    )
    for lit <- literals do
      val expected = java.lang.Double.parseDouble(lit.stripSuffix("d"))
      assertEquals(
        java.lang.Double.doubleToRawLongBits(decoded(lit)),
        java.lang.Double.doubleToRawLongBits(expected),
        s"literal $lit"
      )

  test("fallback literals agree with parseDouble"):
    val literals = Seq(
      "1234567890123456789.5", // 19+ digits: beyond the accumulator
      "1e23",
      "1e-23",
      "4.9e-324",
      "1.7976931348623157e308", // outside |exp10| <= 22
      "1_000.5",                // separator
      "0.30000000000000004"
    )
    for lit <- literals do
      val expected = java.lang.Double.parseDouble(lit.replace("_", ""))
      assertEquals(
        java.lang.Double.doubleToRawLongBits(decoded(lit)),
        java.lang.Double.doubleToRawLongBits(expected),
        s"literal $lit"
      )

  test("negative doubles apply the sign after the exact magnitude"):
    assertEquals(decoded("-12345.99"), -12345.99)
    assertEquals(
      java.lang.Double.doubleToRawLongBits(decoded("-0.0")),
      java.lang.Double.doubleToRawLongBits(-0.0)
    )

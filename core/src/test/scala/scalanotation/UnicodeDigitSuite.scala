package scalanotation

/** Number literals accept unicode digit characters (the scanner's digit class is
  * `Character.isDigit`); their values must interpret through `Character.digit`, never through ASCII
  * arithmetic.
  */
class UnicodeDigitSuite extends munit.FunSuite:
  test("unicode digits interpret correctly in Int literals"):
    assertEquals(Readers.readAs[(x: Int)]("(x = ١٢٣)").toOption, Some((x = 123)))

  test("unicode digits interpret correctly in Long literals"):
    assertEquals(
      Readers.readAs[(x: Long)]("(x = ١٢٣٤L)").toOption,
      Some((x = 1234L))
    )

  test("mixed ascii and unicode digits"):
    assertEquals(Readers.readAs[(x: Int)]("(x = 1٢2)").toOption, Some((x = 122)))

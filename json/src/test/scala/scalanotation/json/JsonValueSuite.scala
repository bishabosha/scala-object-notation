package scalanotation.json

import steps.result.Result

class JsonValueSuite extends munit.FunSuite:
  import JsonValue.*

  private def read(input: String): JsonValue =
    Json.readAs[JsonValue](input).getOrElse(fail(s"failed to parse $input"))

  test("decode every construct"):
    assertEquals(read("null"), JsonValue.Null)
    assertEquals(read("true"), Bool(true))
    assertEquals(read("false"), Bool(false))
    assertEquals(read("\"hi\""), Str("hi"))
    assertEquals(read("42"), Num("42"))
    assertEquals(read("[1,\"a\"]"), arr(Num("1"), Str("a")))
    assertEquals(
      read("""{"a":1,"b":[true,null]}"""),
      obj("a" -> Num("1"), "b" -> arr(Bool(true), JsonValue.Null))
    )
    assertEquals(read("{}"), obj())
    assertEquals(read("[]"), arr())

  test("numbers preserve their exact source text"):
    assertEquals(read("3.141592653589793238462643383279"), Num("3.141592653589793238462643383279"))
    assertEquals(read("123456789012345678901234567890"), Num("123456789012345678901234567890"))
    assertEquals(read("-0.0"), Num("-0.0"))
    assertEquals(read("1e400"), Num("1e400"))
    assertEquals(read("1.0E-7"), Num("1.0E-7"))

  test("numbers round trip without precision loss"):
    val texts = List(
      "3.141592653589793238462643383279",
      "123456789012345678901234567890",
      "-0.000000000000000000001",
      "1e400",
      "0"
    )
    for text <- texts do assertEquals(Json.write(read(text)), text, clue = text)

  test("duplicate keys in objects are preserved order-sensitively rejected"):
    // dynamic objects reject duplicate keys, matching the typed dict semantics
    assert(Json.readAs[JsonValue]("""{"a":1,"a":2}""").isErr)

  test("object field order is preserved"):
    assertEquals(
      read("""{"z":1,"a":2,"m":3}"""),
      obj("z" -> Num("1"), "a" -> Num("2"), "m" -> Num("3"))
    )

  test("dynamic round trips"):
    val value: JsonValue = obj(
      "name"    -> Str("test"),
      "values"  -> arr(Num("1"), Num("2.5"), Str("three"), JsonValue.Null),
      "nested"  -> obj("flag" -> Bool(false)),
      "unicode" -> Str("日本語 😀 \"quoted\"")
    )
    assertEquals(Json.readAs[JsonValue](Json.write(value)), Result.Ok(value))

  test("interpret raw numbers"):
    assertEquals(read("1.5").asInstanceOf[Num].toBigDecimal, BigDecimal("1.5"))
    assertEquals(read("1.5").asInstanceOf[Num].toDoubleValue, 1.5)

  test("numeric constructors"):
    assertEquals(JsonValue.num(42), Num("42"))
    assertEquals(JsonValue.num(42L), Num("42"))
    assertEquals(JsonValue.num(1.5), Num("1.5"))
    assertEquals(JsonValue.num(BigDecimal("1.23")), Num("1.23"))
    intercept[IllegalArgumentException](JsonValue.num(Double.NaN))
    intercept[IllegalArgumentException](JsonValue.numFromText("abc"))
    assertEquals(JsonValue.numFromText("-1.5e3"), Num("-1.5e3"))

  test("writing an invalid raw number fails"):
    intercept[IllegalArgumentException](Json.write[JsonValue](Num("not-a-number")))
    intercept[IllegalArgumentException](Json.write[JsonValue](Num("01")))

  test("pretty dynamic output"):
    assertEquals(
      Json.writePretty[JsonValue](obj("a" -> Num("1"), "b" -> arr(Bool(true)))),
      """|{
         |  "a": 1,
         |  "b": [
         |    true
         |  ]
         |}""".stripMargin
    )

  test("dynamic values decode from bytes"):
    val bytes = """{"a":[1,2],"b":"日本語"}""".getBytes(java.nio.charset.StandardCharsets.UTF_8)
    assertEquals(
      Json.readAs[JsonValue](bytes),
      Result.Ok(obj("a" -> arr(Num("1"), Num("2")), "b" -> Str("日本語")))
    )

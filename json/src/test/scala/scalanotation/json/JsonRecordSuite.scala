package scalanotation.json

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.ReadWriter
import scalanotation.Writer
import steps.result.Result

case class Order(id: Long, sku: String, qty: Int, price: Double, active: Boolean) derives ReadWriter

case class Nested(name: String, inner: Order, tags: Vector[String]) derives ReadWriter

case class Sparse(
    id: Long,
    note: Option[String],
    sku: String,
    retries: Option[Int],
    active: Boolean
)
object Sparse:
  given Reader[Sparse] = Reader.skippable.derived
  given Writer[Sparse] = Writer.derived

case class StrictOrder(id: Long, sku: String, qty: Int, price: Double, active: Boolean)
object StrictOrder:
  given scalanotation.Configured[StrictOrder] =
    scalanotation.Configured.default[StrictOrder].withRejectUnknownFields
  given Reader[StrictOrder] = Reader.configured.derived

class JsonRecordSuite extends munit.FunSuite:

  private def errOf[T](result: Result[T, scalanotation.DecodeError]): scalanotation.DecodeError =
    result match
      case Result.Err(error) => error
      case other             => fail(s"expected an error, got $other")

  private def ok[T](result: Result[T, DecodeError]): T =
    result.getOrElse(fail(s"Expected successful parse, got $result"))

  private val order     = Order(1000L, "sku-1", 3, 100.99, active = true)
  private val orderJson = """{"id":1000,"sku":"sku-1","qty":3,"price":100.99,"active":true}"""

  test("decode a flat record"):
    assertEquals(Json.readAs[Order](orderJson), Result.Ok(order))

  test("decode a record with whitespace everywhere"):
    val spaced =
      """{ "id" : 1000 , "sku" : "sku-1" , "qty" : 3 , "price" : 100.99 , "active" : true }"""
    assertEquals(Json.readAs[Order](spaced), Result.Ok(order))

  test("write a flat record"):
    assertEquals(Json.write(order), orderJson)

  test("record round trip through bytes"):
    assertEquals(Json.readAs[Order](Json.writeBytes(order)), Result.Ok(order))

  test("decode nested records and collections"):
    val nested = Nested("outer", order, Vector("a", "b"))
    val json   = Json.write(nested)
    assertEquals(
      json,
      s"""{"name":"outer","inner":$orderJson,"tags":["a","b"]}"""
    )
    assertEquals(Json.readAs[Nested](json), Result.Ok(nested))

  test("decode named tuples"):
    type Data = (a: Boolean, b: Int, c: String)
    assertEquals(
      Json.readAs[Data]("""{"a":true,"b":-12,"c":"hi"}"""),
      Result.Ok((a = true, b = -12, c = "hi"))
    )

  test("field names with escapes and non-ASCII"):
    type Data = (`weird "name"`: Int, `日本語`: String)
    val value: Data = (`weird "name"` = 1, `日本語` = "ok")
    val json        = Json.write(value)
    assertEquals(json, """{"weird \"name\"":1,"日本語":"ok"}""")
    assertEquals(Json.readAs[Data](json), Result.Ok(value))

  test("missing required fields are an error"):
    val result = Json.readAs[Order]("""{"id":1,"sku":"s","qty":2}""")
    assert(result.isErr)
    assert(errOf(result).format.contains("Missing required field 'price'"))

  test("empty object is an error when fields are required"):
    val result = Json.readAs[Order]("{}")
    assert(result.isErr)
    assert(errOf(result).format.contains("Missing required field 'id'"))

  test("unknown fields are skipped by default"):
    assertEquals(
      Json.readAs[Order](
        """{"id":1000,"bogus":true,"sku":"sku-1","qty":3,"price":100.99,"active":true}"""
      ),
      Result.Ok(order)
    )

  test("unknown fields with structured values are skipped whole"):
    assertEquals(
      Json.readAs[Order](
        """{"id":1000,"junk":{"a":[1,{"b":null}],"c":"x\"y","d":1.5e3},"sku":"sku-1",""" +
          """"qty":3,"trail":[[]],"price":100.99,"active":true}"""
      ),
      Result.Ok(order)
    )

  test("strict configuration rejects unknown fields"):
    val result = Json.readAs[StrictOrder](
      """{"id":1,"sku":"s","bogus":true,"qty":2,"price":1.5,"active":false}"""
    )
    assert(result.isErr)
    assert(errOf(result).format.contains("Unexpected field 'bogus'"), clue = errOf(result).format)
    assertEquals(
      Json.readAs[StrictOrder](orderJson),
      Result.Ok(StrictOrder(1000L, "sku-1", 3, 100.99, active = true))
    )

  test("fields decode in any order"):
    assertEquals(
      Json.readAs[Order](
        """{"sku":"sku-1","active":true,"id":1000,"price":100.99,"qty":3}"""
      ),
      Result.Ok(order)
    )
    // fully reversed
    assertEquals(
      Json.readAs[Order](
        """{"active":true,"price":100.99,"qty":3,"sku":"sku-1","id":1000}"""
      ),
      Result.Ok(order)
    )

  test("out-of-order duplicate fields are an error"):
    val result = Json.readAs[Order](
      """{"sku":"s","id":1,"sku":"s2","qty":2,"price":1.5,"active":false}"""
    )
    assert(result.isErr)
    assert(errOf(result).format.contains("Duplicate field 'sku'"))

  test("duplicate field is an error"):
    val result = Json.readAs[Sparse](
      """{"id":1,"sku":"s","sku":"s2","active":true}"""
    )
    assert(result.isErr)
    assert(errOf(result).format.contains("Duplicate field 'sku'"))

  test("skippable schemas fill omitted optional fields"):
    assertEquals(
      Json.readAs[Sparse]("""{"id":1,"sku":"s","active":true}"""),
      Result.Ok(Sparse(1L, None, "s", None, active = true))
    )
    assertEquals(
      Json.readAs[Sparse]("""{"id":1,"note":"n","sku":"s","retries":3,"active":true}"""),
      Result.Ok(Sparse(1L, Some("n"), "s", Some(3), active = true))
    )
    // bare values decode as Some
    assertEquals(
      Json.readAs[Sparse]("""{"id":1,"sku":"s","retries":7,"active":false}"""),
      Result.Ok(Sparse(1L, None, "s", Some(7), active = false))
    )

  test("skippable schemas reject omitted required fields"):
    assert(Json.readAs[Sparse]("""{"id":1,"active":true}""").isErr)

  test("skippable schemas fill trailing omissions"):
    assertEquals(
      Json.readAs[Sparse]("""{"id":1,"sku":"s","active":true}"""),
      Result.Ok(Sparse(1L, None, "s", None, active = true))
    )

  test("skippable schemas accept any field order"):
    assertEquals(
      Json.readAs[Sparse]("""{"active":true,"retries":3,"id":1,"sku":"s"}"""),
      Result.Ok(Sparse(1L, None, "s", Some(3), active = true))
    )

  test("error paths and spans point at the offending field"):
    val result = Json.readAs[Order]("""{"id":1,"sku":"s","qty":"oops","price":1.5,"active":true}""")
    val error  = errOf(result)
    assert(error.format.contains(".qty"), clue = error.format)

  test("decode error on malformed structure"):
    assert(Json.readAs[Order]("""{"id":1 "sku":"s"}""").isErr)
    assert(Json.readAs[Order]("""{"id":1,}""").isErr)
    assert(Json.readAs[Order]("42").isErr)
    assert(Json.readAs[Order]("""{"id"}""").isErr)

  test("deeply nested input is rejected instead of overflowing the stack"):
    val deep   = "[" * 600 + "]" * 600
    val result = Json.readAs[JsonValue](deep)
    assert(result.isErr)
    assert(
      errOf(result).format.contains("Nesting depth"),
      clue = result.toString
    )

  test("batched decoding with a local context matches one-shot results"):
    given ctx: JsonBatchContext = JsonBatchContext.local()
    var i                       = 0
    while i < 5 do
      assertEquals(Json.batched.readAs[Order](orderJson), Result.Ok(order))
      assertEquals(
        Json.batched.readAs[Order](orderJson.getBytes(java.nio.charset.StandardCharsets.UTF_8)),
        Result.Ok(order)
      )
      i += 1

  test("pretty writing"):
    val pretty = Json.writePretty((a = 1, b = Vector(1, 2)))
    assertEquals(
      pretty,
      """|{
         |  "a": 1,
         |  "b": [
         |    1,
         |    2
         |  ]
         |}""".stripMargin
    )
    assertEquals(
      Json.readAs[(a: Int, b: Vector[Int])](pretty),
      Result.Ok((a = 1, b = Vector(1, 2)))
    )
